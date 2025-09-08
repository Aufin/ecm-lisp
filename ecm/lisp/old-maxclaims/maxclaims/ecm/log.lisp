(defpackage :ecm/log
  (:use :cl)
  (:import-from :ecm/database
		#:with-log-database)
  (:import-from :ecm/user
		#:user
		#:user-database-role
		#:user-password))
(in-package :ecm/log)

(defun log-error (
(defun log-event (&key (log-type "VIEW")
		    (info "")
		    (row-type "")
		    (row-id 0)
		    (user (user)))
  (let ((username (when user (user-database-role user)))
	(password (when user (user-password user))))
    (with-log-database (username password)
      (postmodern:query
       (:SELECT (:insert-user-log row-type row-id log-type info))))))
  
  


