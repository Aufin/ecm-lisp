(defpackage :maxclaims/entry-point/delete
  (:use :cl)
  (:import-from :maxclaims/entry-point/toplevel
		 #:http-parameters-as-alist
		 #:http-parameters-changed-alist
		 #:select-object-from-request
		 #:object-typename-and-id-from-request)
  (:import-from :maxclaims/web-display/html-page
		  #:get-app-user
		  #:with-user-html-page)
  (:import-from :maxclaims		
		#:call-with-app-user)
   (:import-from :maxclaims/hunchentoot
		#:define-easy-handler
		#:redirect)
  (:import-from :maxclaims/data-entity/app-user
		:app-user-administrator-p)
  (:import-from #:maxclaims/web-display/display
		#:<view-link))
  

(in-package :maxclaims/entry-point/delete)


(define-easy-handler (|delete| :uri "/ecm/delete")
    ((confirm :real-name "delete[confirm]")
     (access-type :real-name "access[type]")
     (access-id :real-name "access[id]")
     (active-tab :real-name "back[active-tab]"))
  (let ((object (call-with-app-user 
		 (get-app-user) 
		 #'select-object-from-request))
	#+(or)	(method (ucw-core::http-method
			 (ucw-core:context.request 
			  ucw-core:*context*))))
    (flet ((delete-page (&optional condition)
	     (with-user-html-page 
		 (:title (if condition "Cannot Delete"
			     "DELETE? : ECM"))
	       (maxclaims::with-udb 
		 (<:as-is '#:|
   <style type="text/css">
      body {
        padding-top: 40px;
        padding-bottom: 40px;
        background-color: #f5f5f5;
      }

      .form-signin {
        max-width: 500px;
        padding: 19px 29px 29px;
        margin: 0 auto 20px;
        background-color: #fff;
        border: 1px solid #e5e5e5;
        -webkit-border-radius: 5px;
           -moz-border-radius: 5px;
                border-radius: 5px;
        -webkit-box-shadow: 0 1px 2px rgba(0,0,0,.05);
           -moz-box-shadow: 0 1px 2px rgba(0,0,0,.05);
                box-shadow: 0 1px 2px rgba(0,0,0,.05);
      }
      .form-signin .form-signin-heading,
      .form-signin .checkbox {
        margin-bottom: 10px;
      }
      .form-signin input[type="text"],
      .form-signin input[type="password"] {
        font-size: 16px;
        height: auto;
        margin-bottom: 15px;
        padding: 7px 9px;
      }

    </style>|)
		 (<:form 
		  :class "form-signin"
		  :action "#"
		  :method :post
		  (<:input :type "submit"
			   :name "delete[confirm]"
			   :class "btn btn-success"
			   :value "Delete")
		 
		  (<view-link 
		      (object :class "btn btn-danger"
			      :style "float:right;")
		    (<:as-html "Cancel"))
		  (<:hr)
		  (<:h1 (<:as-html 
			 (if condition "Cannot Delete:" "Really Delete:")))
		  (when condition
		    (<:big (<:as-html  (princ-to-string condition))))
		  (maxclaims/web-display/display:display object :heading)
		  (maxclaims/web-display/display:display object :view)
			 (<:input :type "hidden"
				  :name "access[type]"
				  :value access-type)
			 (<:input :type "hidden"
				  :name "access[id]"
				  :value access-id)
			 (when active-tab 
			   (<:input :type "hidden"
				    :name "back[active-tab]"
				    :value active-tab))
			 (<:hr)
			 
			 (<:input :type "submit"
				  :name "delete[confirm]"
				  :class "btn btn-success"
				  :value "Delete")
		 
			 (<view-link 
			     (object :class "btn btn-danger"
				     :style "float:right;")
			   (<:as-html "Cancel")))
		 (<:div 
		
			)))))
      
      (if (and object (app-user-administrator-p (get-app-user)))
	  (if confirm 
	      (call-with-app-user 
	       (get-app-user)
	       (lambda ()	
		 (maxclaims::with-udb 
		   (handler-case (progn
				   (rofl:delete-object object)
				   (redirect 
				    (format nil "/ecm/view?~A=~A&~{tab[active]=~A~}"
					   access-type access-id 
					   (when (and active-tab
						      (not (string-equal "nil" active-tab)))
					     (list active-tab)))))
		     (error (c)
		       (if maxclaims::*debug-on-error*
			   (error c)
			   (delete-page c)))) 
		   )))
	      (delete-page)
	      )
		    
	    
	  (<:as-html "Nothing to Delete")))))


  
