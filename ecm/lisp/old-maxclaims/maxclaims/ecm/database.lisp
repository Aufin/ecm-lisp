(uiop:define-package :ecm/database
  (:use :cl :postmodern)
  (:import-from :ecm/user
		#:user-password
		#:user-database-role
		#:user)
  (:reexport :postmodern)
  (:import-from #:ecm/json
		#:read-json-from-string)
  (:import-from :maxclaims
		#:*db-connection-parameters*
		#:*db-admin-parameters*)
  (:export #:query/return-jso
	   #:query 
	   #:*database*
	   #:call-with-database
	   #:with-database
	   #:call-with-dynamic-log-database
	   #:with-log-database))
(in-package :ecm/database)

(defun call-with-database (function &key (pooled t))
  (with-connection
      (append *db-connection-parameters*
	      `(:pooled-p ,pooled))
    (funcall function *database*)))

(defmacro with-database ((&key (name '*database*) (pooled t))
			 &body body)
  `(call-with-database (lambda (,name) ,@body)
			  :pooled ,pooled))
  
(defmacro query/return-jso (&body query+args)
  "=> jso or nil"
  (let ((json (gensym)))
    `(let ((,json
	    (postmodern:query
	     ,@query+args
	     :single)))
       (when (and ,json (not (eql :null ,json)))
	 (ecm/json:read-json-from-string ,json)))))

(defun call-with-dynamic-log-database (thunk &key username password)
  (destructuring-bind (db user pw host &key port)
      *db-connection-parameters*
    (declare (ignore db))
    (let ((database "maxclaims_log"))
      (postmodern:with-connection (list database
					(or username user)
					(or password pw)
					host :port port)
	(funcall thunk)))))

(defmacro with-log-database ((&optional username password) &body body)
  `(call-with-dynamic-log-database
    (lambda () ,@body)
    :username ,username :password ,password))
