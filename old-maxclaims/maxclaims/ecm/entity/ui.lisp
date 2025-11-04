(defpackage :ecm/entity/ui
  (:use :cl)
  (:import-from :ecm/json
		#:getjso #:jso)
  (:import-from :ecm/user)
  (:import-from :ecm/database)
    
  (:export #:user-claim-ui))
(in-package :ecm/entity/ui)

(defun user-claim-ui (claim-id &optional (user (ecm/user:user)))
;  (break "~A" user)
  (ecm/database:query/return-jso
    (:SELECT (:ui.claim claim-id (ecm/user:user-id user)))))

(defun (setf user-claim-ui) (value claim-id &optional (user (ecm/user:user)))
  (let (set)
    (user-claim-ui claim-id user)
    (ecm/json:mapjso
     (lambda (key val)
	     (setf key (read-from-string key))
	     (push key set)
	     (push
	      (if (member val '(:true :false))
		  (ecm/json:from-json-bool val)
		  val)
	      set))
     value )
    (postmodern:query
     (s-sql:sql-compile
      `(:update ui.claim :set ,@(nreverse set)
		:where (:and (:= claim-id ,claim-id)
			     (:= app-user-id
				 ,(ecm/user:user-id user))))))
    (user-claim-ui claim-id user)))
		     
  
