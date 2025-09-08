(defpackage :maxclaims/log/user-log
  (:use :cl )
  (:import-from :maxclaims
		#:with-ldb
		#:select-limit*
		#:app-user
		#:app-user.app-user-id
		#:app-user-id-rolename
		#:app-user-rolename
		#:app-user.password
		#:*db-connection-parameters*
		#:with-db)
		
  (:export #:with-log-database
	   #:select-user-distinct-log))
(in-package :maxclaims/log/user-log)

(defclass user-log ()
  ((user-log-id :primary-key t)
   row-type
   row-id
   user-role
   log-time
   log-type
   log-info)
  (:metaclass maxclaims::described-db-access-class))

(defmacro with-log-database ((&key app-user) &body body)
  (let ((app-usern (gensym)))
    `(let* ((,app-usern ,app-user)
	    (*db-connection-parameters* 
             (copy-list *db-connection-parameters*)))
       (setf (first *db-connection-parameters*)
             "maxclaims_log"
             
             (second *db-connection-parameters*) 
             (if ,app-usern (app-user-rolename ,app-usern) "mrl_lookup_user")
             
             (third  *db-connection-parameters*)
             (if ,app-usern (app-user.password ,app-usern) "qu0hhu@n")

	     #+(or)	     (first (last *db-connection-parameters*))
	     #+(or) 5432
	     )
       (with-db 	       	
	 ,@body))))

(defun select-user-distinct-log (user &key log-type)
  (with-log-database ()
    (rofl:query-objects 
     'user-log 
     (lambda (&rest _)
       (declare (ignore _))
       `(:raw ,
	 (format nil "
SELECT * FROM (SELECT DISTINCT ON (~A) log_info, 
                      log_type, log_time, row_type, row_id
               FROM user_log 
               WHERE log_type ILIKE '~A' 
               AND user_role = '~A'
               ORDER BY ~A,log_time DESC ) AS log 
               ORDER BY log_time DESC LIMIT 25;"
		 (if (string-equal log-type "search")
		     "log_info"
		     "row_type, row_id")
	       (or log-type "search")
	       (app-user-id-rolename
		(typecase user
		  (number user)
		  (app-user (app-user.app-user-id user))))
	       (if (string-equal log-type "search")
		     "log_info"
		     "row_type, row_id")))))))


  

