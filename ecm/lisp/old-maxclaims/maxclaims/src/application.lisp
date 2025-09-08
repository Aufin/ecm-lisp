(in-package :maxclaims)

(defclass maxclaims-server (standard-server)
  ())



(defclass maxclaims-application (static-roots-application-mixin
				 cookie-session-application-mixin)
  ()
  (:default-initargs 
   :static-roots (list (cons "static/" (project-relative-pathname #P"wwwroot/"))
			(cons "csv-reports/" nil))))



(defvar *backend-parameters* nil 
  "will be set from configuration file")

(defun make-maxclaims-backend ()
  (apply #'ucw-core::make-backend
	 (append *backend-parameters*
		`(:request-content-length-limit
		  ,(* 1024 1024 1024)))))


(defun make-maxclaims-server ()
  (make-instance
   'maxclaims-server
   :backend (make-maxclaims-backend)))





