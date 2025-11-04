(in-package :maxclaims)

(defvar *db-connection-parameters* )
(defvar *db-admin-parameters*)

(defun object-table-name (object)
  (s-sql:sql-escape 
   (rofl::class-table-name (class-of object))))

(defun object-primary-key-name (object)
  (c2mop:slot-definition-name (rofl::class-id-slot-definition (class-of object))))

(defun class-primary-key-name (class)
  (c2mop:slot-definition-name (rofl::class-id-slot-definition class)))

(defun select-limit* (limit offset query)
  (rofl::%query `(:limit ,query ,limit ,offset)))

(defun select* (query)
  (rofl::%query query))

(defun app-user-id-rolename (user-id)
  (string-downcase (format nil "mu_~A" user-id)))

(defun app-user-role->app-user-id (user-rolename)
  (let ((user-id-as-string (second (split-sequence #\_ user-rolename :from-end t))))
    (when user-id-as-string
      (parse-integer user-id-as-string))))

(defun funcall-with-db (thunk)
  (ecm/postgresql/connection:call-with-connection
   *db-connection-parameters* thunk))

(defmacro with-db (&body body)
  `(funcall-with-db (lambda () ,@body)))

(defmacro with-adb (&body body)
  `(let ((*db-connection-parameters* *db-admin-parameters*))
     (with-db
       ,@body)))

(defmacro with-udb (&body body)
  `(let* ((app-user $app-user)
	  (*db-connection-parameters*
      (copy-list *db-connection-parameters*)))
     (setf (second *db-connection-parameters*)
           (if app-user (app-user-rolename app-user) "mrl_lookup_user"))
     (with-db
       ,@body)))

(defmacro with-ldb (&body body)
  `(let* ((app-user $app-user)
	  (*db-connection-parameters* (copy-list *db-connection-parameters*)))
     (setf (first *db-connection-parameters*)
           "maxclaims_log"
           (second *db-connection-parameters*)
           (if app-user (app-user-rolename app-user) "mrl_lookup_user"))
     (with-db
       ,@body)))
