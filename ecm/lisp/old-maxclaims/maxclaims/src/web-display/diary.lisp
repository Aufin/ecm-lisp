(defpackage :maxclaims/web-display/diary
  (:use :cl :maxclaims/yaclml/tags)
  
  (:import-from :maxclaims/web-display/display
		#:activate-lambda
		#:<object-link
		#:*current-tab*
		#:object-name-and-pkey-value
		;#:<view-link

					;		#:<delete-link
					;		#:get-app-user
		)
		#+ (or) (:import-from :maxclaims 
				      #:with-udb
				      #:call-with-app-user))
(in-package :maxclaims/web-display/diary)

(defmethod activate-lambda 
    ((name (eql 'maxclaims/ecm-description:diary-link)))
  (lambda (o f) 
    (ignore-errors
      (let ((diary (slot-value 
		    o 'maxclaims/ecm-description::object)))
	 (unless (maxclaims::diary-entry.processedp
		  diary)
	   	(<:p 
		 
		 (<object-link 
		     (:object 
		      diary
		      :place "edit" 
		      :tag-attributes 
		      (:class "btn btn-success")
		      :uri-attributes
		      `(("future[processed]" t)
			("access[type]" "claim")
			("access[id]" ,(maxclaims::diary-entry.claim-id diary))
			,@(when *current-tab* 
				`(,(list "back[active-tab]"
					 *current-tab*)))))
		   (<:as-html "mark done"))
	  
		 (<:as-html " ")
		 (<:a :href (format nil  
				    "create?create[type]=defer-diary-entry&create[key]=diary-entry-id&access[type]=diary-entry&access[id]=~A&back[active-tab]=Deferred"
				    (cdr (object-name-and-pkey-value diary)))
		      :class "btn btn-warning"
		      (<:as-html "defer")))

	   )

	
		     
	(<:div 
	 (when (= (maxclaims::diary-entry.app-user-id diary)
		  (maxclaims::app-user.app-user-id
		   maxclaims::$app-user))
		 (<object-link 
		    (:object 
		     diary
		     :place "edit" 
		     :tag-attributes 
		     (:class "btn btn-success")
		     :uri-attributes
		     `(
		       ("access[type]" "claim")
		       ("access[id]" ,(maxclaims::diary-entry.claim-id diary))
		       ,@(when *current-tab* 
			       `(,(list "back[active-tab]"
					*current-tab*)))))
		   (<:as-html "Edit")))
	 (<:as-html " ")
	 (funcall f)
	       
	       )
		      
	))))
