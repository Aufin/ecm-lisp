(in-package :maxclaims)

(defcomponent object-creator-component (object-viewer-component)
  ()
  (:metaclass described-component-class))

;; (defaction insert-object-action :around ((self object-creator-component))
;;   #+nil (let ((duplicates (search-for-duplicates (object self))))
;;     (if duplicates 
;; 	(call 'duplicate-record-search-results :data duplicates)
;; 	(call-next-method)))

;;   (call-next-method))

(defmethod render ((self object-creator-component))
  (with-active-descriptions (editable validate object-creator-component)
    (let ((submit-action 
	   (make-action 
	    (lambda ()
	      (arnesi:with-call/cc  
		(when (lol:validp (object self))		  
		  (prog1 (insert-object-action self)
		  ;  (break "Insert object answered")
		    #+nil (log-viewer-object :object (object self)
				       :log-type "INSERT")
		    ;(break "~A" 'log-viewer)
		    ))))
	    :class 'maxclaims-standard-action)))
      (<ucw:form :action* submit-action 
		 :method "POST"
		 :enctype "multipart/form-data"
		 (<:submit)
		 (<ucw:submit :action (answer nil)
			      "Cancel")
		 (read-object (object self))
		 (<:submit)
	         (<ucw:submit :action (answer nil)
			      "Cancel")))))

(defun find-foreign-objects (object)
  (let* ((class (class-of object))) 
    (mapcar (lambda (x)
	      (and (lol::slot-boundp-using-class class object x)  
		   (lol::slot-value-using-class class object x)))
	    (remove-if-not #'rofl::slot-definition-foreign-type 
			   (lol::class-slots class)))))


(defaction insert-object-action (self) 
  (insert-object-action-using-object self (object self)))

(defaction insert-object-action-using-object (self object)
  (let* ((class (class-of (object self))) 
	 (foreign-objects (find-foreign-objects (object self))))
    (let ((it-be t))
      (dolist (o foreign-objects)
	(unless (or (not (rofl::db-access-object-p o))
		    (primary-key-boundp o))
	  (unless (create-object o)
	    (setf it-be nil))))
      (when (and it-be (notany #'consp foreign-objects))      
	(if (primary-key-boundp (object self))
	    (update-object-action self)
	    (answer (insert-with-history (object self))))))))

					 
(defaction create-object (object)
  (call 'object-creator-component :object object))



