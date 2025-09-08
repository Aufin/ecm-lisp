(defpackage :maxclaims/entry-point/view-tabs
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
  (:import-from :maxclaims/web-display/view-tabs
		#:view-tabs-page)
  (:import-from :maxclaims/hunchentoot
		#:define-easy-handler))
  
(in-package :maxclaims/entry-point/view-tabs)


(define-easy-handler (|view-tabs| :uri "/ecm/view-tabs")
    ((active-tab :real-name "tab[active]")
     (view-tab :real-name "tab[view-tab]"))
  (when view-tab
    (setf view-tab
	  (intern view-tab :keyword)))
  (let ((object (ignore-errors (call-with-app-user 
				(get-app-user) 
				#'select-object-from-request)))
	(maxclaims/web-display/display::*view-name-and-pkey-value*
	 (ignore-errors (object-typename-and-id-from-request)))
	(offsets (http-parameters-as-alist "offset")))
					;   (break "object: ~A~% name/pkey : ~A" 
					;	   object maxclaims/web-display/display::*view-name-and-pkey-value*)

    (when (string= (first maxclaims/web-display/display::*view-name-and-pkey-value*)
		   "manage-viewer")
      (setf object (make-instance (intern "MANAGE-VIEWER" 
					  :maxclaims/ecm-description))))
    
    (if object 
	(apply #'view-tabs-page object 
	        ; :http-query-string (http-query-string)
	       :offsets offsets
	       (nconc 
		(when active-tab 
		  `(:active-tab ,active-tab))
		(when view-tab
		  `(:view-tab ,view-tab)))))))

