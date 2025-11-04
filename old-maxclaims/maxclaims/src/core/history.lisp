(in-package :maxclaims)

(defun make-history-data (object)
  (mapcar (lambda (object) 
		     (if (typep object 'simple-date:date)
			 (cl-postgres:to-sql-string object)
			 object))
		   (rofl::make-insert-object-plist object)))

(defun insert-history (object)
  
  (insert-object 
   (make-object 'history
		:object-type (symbol-name (type-of object))
		:object-id (rofl::object-id object)
		:app-user-id (app-user.app-user-id  $app-user)
		:history-data (format nil "~S" (make-history-data object)))))

(defun insert-with-history (object)
  (prog1 (insert-object object)
    (insert-history object)))

(defun update-with-history (object)
  (prog1 (update-object object)
    (insert-history object)))

