(defpackage :maxclaims/entry-point/view
  (:use :cl)
  (:import-from :maxclaims/entry-point/toplevel
		#:http-query-string
		#:select-object-from-request
		#:object-typename-and-id-from-request
		#:http-parameters-as-alist)
  (:import-from :maxclaims/web-display/display
		  #:display)
  (:import-from :maxclaims/web-display/html-page
		  #:get-app-user)
  (:import-from :maxclaims
		#:with-udb
		#:call-with-app-user)
  (:import-from :maxclaims/web-display/view
		#:view-page
		#:view-history-page)
  (:import-from :maxclaims/hunchentoot
		#:define-easy-handler
		#:redirect))
  
(in-package :maxclaims/entry-point/view)


(define-easy-handler (view :uri "/ecm/view")
    ((active-tab :real-name "tab[active]")
     (manage-viewer)
     (go-back-to-claim)
     (old-view :init-form nil))
  (let ((maxclaims/web-display/display::*view-name-and-pkey-value*
	 (ignore-errors (object-typename-and-id-from-request))))
  (cond
      (manage-viewer
             (let ((manage-url (apply #'concatenate 
		       'string "/ecm/manage" 
		       (when active-tab
			 (list "?tab[active]=" active-tab)))))
	
	       (redirect manage-url)))
      ((and (not old-view)
	    (string-equal "claim"
			  (first maxclaims/web-display/display::*view-name-and-pkey-value*)))

       (redirect (concatenate 'string "/ecm/claim/"
			      (second  maxclaims/web-display/display::*view-name-and-pkey-value*))))
      #+(or)((and (not old-view)
	    (string-equal "diary-entry"
			  (first maxclaims/web-display/display::*view-name-and-pkey-value*)))

       (redirect (concatenate 'string "/ecm/diary-entry/"
			      (second  maxclaims/web-display/display::*view-name-and-pkey-value*))))
      ((and (not old-view)
	    (string-equal "risk"
			  (first maxclaims/web-display/display::*view-name-and-pkey-value*)))

       (redirect (concatenate 'string "/ecm/risk/"
			      (second  maxclaims/web-display/display::*view-name-and-pkey-value*))))
      ((and (not old-view)
	    (string-equal "timecard"
			  (first maxclaims/web-display/display::*view-name-and-pkey-value*)))

       (redirect (concatenate 'string "/ecm/timecard/"
			      (second  maxclaims/web-display/display::*view-name-and-pkey-value*))))
    (t 
      (let ((object (handler-case (call-with-app-user 
				   (get-app-user) 
				   #'select-object-from-request)
		      		      
		      (error (c)
			(if maxclaims::*debug-on-error* 
			    (error c)
			    c))

		      ))	    
	    (offsets (http-parameters-as-alist "offset")))
	;;    (break "~A" object)
	(cond ((not object) 
	       (view-history-page 
		(make-condition 'simple-error 
				:format-control (format NIL "Access has been declined. Please contact the system administrator for further details" ))))
	      ((not (typep object 'cl:error))
	       (apply #'view-page object 
		      :http-query-string (http-query-string)
		      :offsets offsets
		      :go-back-to-claim go-back-to-claim
		      (when active-tab 
			(list :active-tab active-tab))))
	      (t  (view-history-page 
		   (etypecase object
		     (simple-error nil)
		     (t object))))))))))

