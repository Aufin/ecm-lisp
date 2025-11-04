(in-package :maxclaims)

(defclass maxclaims-view-claims-component ()
  ((offset :accessor offset
	   :initform 0)
   (limit :accessor limit
	  :initform 20)
   (user :accessor user
	 :initform nil))
  (:metaclass described-component-class))

(defmethod render ((self maxclaims-view-claims-component))
  (or (user self)
      (setf (user self)
	    (list (app-user.app-user-id $app-user)
		  (app-user.username $app-user))))
  (render-claims-view self $body))

(defun render-claims-view (component &optional (source-component component))
  (let ((*source-component* source-component)
	(history-links
	  (with-ldb 
	    (select-limit*
	     (limit component)
	     (offset component)
	     `(:order-by (:select row-id row-type log-type log-info
				  :distinct-on row-id
				  :from user-log 
				  :where (:and (:= user-role 
						   ,(app-user-id-rolename
						     (car (user component))))
					       (:= log-type "VIEW")
					       (:= row-type "claim"))
				  :group-by row-id row-type log-type log-info log-time)
				 
				  
				       
			 (:desc row-id)
			 (:desc log-time))))))
    (let (claim-id error)
      (<ucw:form 
       :action (when (numberp claim-id)
		 (let ((object 
			 (car (select-objects 'claim :where `(:= claim-id ,claim-id)))))
		   (if object 
		       (view-object object)
		       (setf error "Not a valid claim number"))))
	
       (<:as-html "View Claim Number :")
       (<ucw:input :reader claim-id
		   :size 7
		   :writer (lambda (v)
			     (let ((i (parse-integer v :junk-allowed t)))
			       (if (numberp i)
				   (setf claim-id i 
					 error nil)
				   (setf error "Not a valid claim number")))))
       (<:submit :value "View"))
       (when error 
	 (<:span "font-color:red" (<:as-html error))))
				 
	       
		
    (<:ul
     (dolist (r history-links)
       (<:li 
	(render-user-history-link (let ((type (getf r :row-type)))
				    (if (string= type "")
					type 
					(intern (string-upcase 
						 (substitute #\- #\_ (getf r :row-type))) 
						(find-package :maxclaims))))
				  (intern (getf r :log-type))
				  r))))
    (when (= (length history-links) 
	     (limit component))
    
      (<ucw:a :action (setf (offset component) 
			    (+ (offset component)
			       (limit component)))
	      (<:as-html "View Next Claims")))
    (when (> (offset component) 0)
      (<:as-html "  ")
      (<ucw:a :action (when (> (offset component) 0)
			(setf (offset component) 
			      (- (offset component)
				 (limit component))))
	      (<:as-html "View Previous Claims")))))