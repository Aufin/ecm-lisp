(in-package :maxclaims)

(defclass maxclaims-user-history-component ()
  ((offset :accessor offset
	   :initform 0)
   (limit :accessor limit
	  :initform 10)
   (user :accessor user
	 :initform nil))
  (:metaclass described-component-class))

(defmethod render ((self maxclaims-user-history-component))
  (or (user self)
      (setf (user self)
	    (list (app-user.app-user-id $app-user)
		  (app-user.username $app-user))))
  (render-user-history self $body))

(defmethod render-user-history-link (row-type log-type row)
  (<:as-html (getf row :log-info)))

(defmethod render-user-history-link ((row-type string)
				     log-type row)
  (<:as-html (getf row :log-info)))

(defun render-view-object-link (type pkey id next-method)
  (<ucw:a :action  (let ((object 
			   (car (select-objects type :where `(:= ,pkey ,id)))))
		     (view-object object))
	  (funcall next-method)))
  
(defmethod render-user-history-link ((row-type symbol)
				     log-type row)
  (render-view-object-link row-type (class-primary-key-name (find-class row-type)) 
			   (getf row :row-id)
			   (lambda () (call-next-method))))

(defun render-user-history (user-history-component source-component)
  (let ((*source-component* source-component)
	(history-links
	  (with-ldb 
	    (select-limit*
	     (limit user-history-component)
	     (offset user-history-component)
	     `(:order-by (:select '* :from user-log 
			  :where (:and (:= user-role 
					   ,(app-user-id-rolename
					     (car (user user-history-component))))
				       (:not (:= log-type "LOGIN SUCCESS"))))
			
			
			(:desc log-time))))))
		
    (<:ul
    (dolist (r history-links)
      (<:li (<:as-html (getf r :log-type) " ")
	    (render-user-history-link (let ((type (getf r :row-type)))
					(if (string= type "")
					    type 
					    (intern (string-upcase 
						     (substitute #\- #\_ (getf r :row-type))) 
						    (find-package :maxclaims))))
				      (intern (getf r :log-type))
				      r))))
    (when (= (length history-links) 
	     (limit user-history-component))
    
      (<ucw:a :action (setf (offset user-history-component) 
			    (+ (offset user-history-component)
			       (limit user-history-component)))
	      (<:as-html "View Next Actions")))
    (when (> (offset user-history-component) 0)
      (<:as-html "  ")
      (<ucw:a :action (when (> (offset user-history-component) 0)
			(setf (offset user-history-component) 
			      (- (offset user-history-component)
				 (limit user-history-component))))
	      (<:as-html "View Previous Actions")))))
      
	
				  
    
    
    
