(defpackage :ecm/postgresql/authentication
   (:use :cl)
   (:import-from :cl+ssl)
   (:import-from :cl-postgres)
   (:import-from :uiop/run-program)
   (:import-from :postmodern)
   (:import-from :cl-fad)
   (:export #:call-with-dynamic-postgresql-authentication-keys))
(in-package :ecm/postgresql/authentication)

(defvar *cn-escape-chars* "/")
(defun create-cn (username)
  (with-output-to-string (s)
    (princ "/CN=ecm\\/user\\/" s)
    (map nil (lambda (c)
               (when (find c *cn-escape-chars* :test #'eql)
                 (princ #\\ s))
               (princ c s))
         username)))

(defun conf.d ()
  (merge-pathnames ".ecm/" (user-homedir-pathname)))


(defun create-auth-keys (pg-username
                         &key
                           (conf.d (merge-pathnames "postgresql/ssl/" (conf.d)))
                           (user.d (merge-pathnames "users/" conf.d)))
  (labels ((fname (dir ext &optional (name pg-username))
             (princ-to-string
              (merge-pathnames (make-pathname :name name :type ext)
                               dir)))
           (csr ()
             (uiop:run-program `("openssl" "req" "-new" "-nodes" "-text"
                                           "-out" ,(fname user.d "csr")
                                           "-keyout" ,(fname user.d "key")
                                           "-subj" ,(create-cn pg-username))
                               :output :string :error-output :output))
           (crt () 
             (uiop:run-program `("openssl" "x509" "-req"
                                           "-in" ,(fname user.d "csr") "-text" "-days" "3650"
                                           "-CA" ,(fname conf.d "crt" "root")
                                           "-CAkey" ,(fname conf.d "key" "root")
                                           "-CAcreateserial" "-out" ,(fname user.d "crt"))
                               :output :string :error-output :output)))
    (let ((csr (csr))
          (crt (crt)))
      (concatenate 'string csr crt))))

(defun find-auth-keys (username
                       &key
                         (conf.d (merge-pathnames "postgresql/ssl/" (conf.d)))
                         (user.d (merge-pathnames "users/" conf.d)))
  "=> /values/ key, crt"
  (let ((key (merge-pathnames (make-pathname :name username :type "key") user.d))
        (crt (merge-pathnames (make-pathname :name username :type "crt") user.d)))
    (when (every #'cl-fad:file-exists-p (list key crt))
      (values key crt))))

(defun ensure-auth-keys (username
                         &key
                           (conf.d (merge-pathnames "postgresql/ssl/" (conf.d)))
                           (user.d (merge-pathnames "users/" conf.d))
                           (error nil))
  (multiple-value-bind (key crt)
      (find-auth-keys username :conf.d conf.d :user.d user.d)
    (cond ((and (not key) (not error))
           (let ((output (create-auth-keys username :conf.d conf.d :user.d user.d)))
             (ensure-auth-keys username :conf.d conf.d :user.d user.d
                                        :error output)))
          ((and (not key) error)
           (error "Cannot create keys: ~A" error))
          (t (values key crt)))))

(defun call-with-dynamic-postgresql-authentication-keys
    (username thunk &key
                      (conf.d (merge-pathnames "postgresql/ssl/" (conf.d)))
                      (user.d (merge-pathnames "users/" conf.d)))
 ;;; NO MORE SSL! YAY!
  #+(or)(multiple-value-bind (key crt)
			(ensure-auth-keys username :conf.d conf.d :user.d user.d)
		  (let ((cl-postgres:*ssl-key-file* (princ-to-string key))
				(cl-postgres:*ssl-certificate-file* (princ-to-string crt))
				(cl+ssl:*make-ssl-client-stream-verify-default* nil))
			(funcall thunk)))
  (funcall thunk))
