(defpackage :ecm/configuration
  (:use :cl)
  (:import-from :cl-fad)
  (:import-from :ecm/json)
  (:export
   #:attachment-directory
   #:archive-directory
   #:find-configuration-file
   #:configuration-file-possible-directories
   #:configuration-value
   #:load-configuration-file))
(in-package :ecm/configuration)

 
(defvar *configuration-file-json*)
(defvar *default-configuration-file-json*)

(defun getconf (key)
  (multiple-value-bind (conf found?)
      (and *configuration-file-json*
	   (ecm/json:getjso* key *configuration-file-json*))
    (if found?
	(values conf found?)
	(and *default-configuration-file-json*
	     (ecm/json:getjso* key *default-configuration-file-json*)))))

(defun configuration-file-possible-directories ()
  (list (merge-pathnames ".ecm/" (user-homedir-pathname))
	(make-pathname :directory '(:absolute "ecm" "etc"))
	(make-pathname :directory '(:absolute "home" "maxclaims" "ecm" "etc"))))

(defun find-configuration-file (&key (filename "ecm.json")
				  (possible-directories (configuration-file-possible-directories)))
  (let* ((dirs (remove-if-not #'cl-fad:directory-exists-p
			     possible-directories))
	 (files (remove-if-not
		 #'cl-fad:file-exists-p
		 (mapcar (lambda (dir)
			   (merge-pathnames filename dir))
			 dirs))))
    (first files)))

(defun load-configuration-file ()
  (flet ((givr (file &key (dirs (configuration-file-possible-directories)))
	   (let ((f (find-configuration-file
		     :filename file
		     :possible-directories dirs)))
	     (when f (ecm/json:read-json f)))))
    (prog1
	(setf *configuration-file-json* (givr "ecm.json"))
      (setf *default-configuration-file-json* (givr "ecm.json"
						    :dirs (list (make-pathname :directory '(:absolute "ecm" "etc"))))))))
 
(defgeneric configuration-value (key &key error &allow-other-keys)
  (:method :before (key &key &allow-other-keys)
	   (unless (boundp '*configuration-file-json*)
	     (load-configuration-file)))
  (:method :around (key &key (error t error-provided?) &allow-other-keys)
	   (multiple-value-bind (v found?)
	       (call-next-method)
	     (if (and (not v) (not found?) (or error (not error-provided?)))
		 (error "Cannot find ~S in configuration" key)
		 (values v found?))))	   
  (:method ((key string) &key &allow-other-keys)          
    (getconf key))
  (:method ((key symbol) &rest args &key &allow-other-keys)
    (apply #'configuration-value (let ((*print-case* :downcase))
				   (princ-to-string key))
	   args)))


(defun attachment-directory ()
  (cl-fad:pathname-as-directory (configuration-value "attachment.directory")))

(defun archive-directory ()
  (cl-fad:pathname-as-directory (configuration-value "archive.directory")))
  
  
;; (defvar *attachment-path*
;;   (make-pathname 
;;    :directory (list :absolute
;; 		    "home" "maxclaims" "attachment")))


;; (defparameter *configuration-file-directories*
;;   #'user-homedir-pathname)


;; (defvar *archive-path*
;;   (make-pathname 
;;    :directory (list :absolute
;; 		    "home" "maxclaims" "archive")))

;; (defvar *attachment-mirror-path*
;;   (make-pathname 
;;    :directory (list :absolute
;; 		    "home" "maxclaims" "attachment-mirror")))
