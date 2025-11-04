(in-package :maxclaims)

(defvar *history-user-alist* NIL)

(define-description history.hstore-history (description-for-history.hstore-history)
  ((username :function 
	     (lambda (object)
	       (let ((user (history.hstore-history.user-role object)))
		 (or (cdr (assoc user *history-user-alist* :test #'string=))
		     (let ((app-user 
			     (with-db 
			       (select-only-n-objects 
				1
				'app-user 
				:where `(:= app-user-id 
					    ,(or (app-user-role->app-user-id user) 
						 1))))))
		       (setf *history-user-alist* 
			     (acons user app-user *history-user-alist*))
		       app-user))))
	     :attributes ((username :label nil))
					       
	     :label "User")))

(define-description history.hstore-history (description-for-history.hstore-history)
  ((active-attributes :value '(modification
			       username
			       modification-time
			       (history ) previous)))
  (:in-description inline))

(define-description history (description-for-history)
  ((object :label "Record"
	   :function 'history-object)))

(define-description history (description-for-history)
  ((active-attributes :value '(object-type object-id modification-time)))
  (:in-description inline))

(defun history-object (history-record)
  (let ((object-class (find-class (intern (history.object-type history-record)
					  :maxclaims))))
    (select-only-n-objects 1 (class-name object-class)
			   :where `(:= ,(rofl::class-id-column-name object-class)
				       ,(history.object-id history-record)))))