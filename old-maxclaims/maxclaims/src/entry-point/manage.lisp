(defpackage :maxclaims/entry-point/manage
  (:use :cl)
  (:import-from :maxclaims/entry-point/toplevel
		#:select-object-from-request
		#:object-typename-and-id-from-request
		#:http-parameters-as-alist)
  (:import-from :maxclaims/web-display/html-page
		  #:get-app-user)
  (:import-from :maxclaims
		#:with-udb
		#:call-with-app-user
		#:app-user.admin)
  (:import-from :maxclaims/hunchentoot
		#:define-easy-handler
		#:redirect)

  ;;; display
  (:import-from :maxclaims/web-display/view
		#:view-page)
  (:import-from :maxclaims/web-display/navbar
		#:navbar)
  (:import-from :maxclaims/web-display/display
		#:display))
  
(in-package :maxclaims/entry-point/manage)


(define-easy-handler (|manage| :uri "/ecm/manage")
    ((active-tab :real-name "tab[active]"))
  (let ((manager? (ignore-errors (app-user.admin (get-app-user)))))    
    (maxclaims/web-display/html-page:with-user-html-page 
	  (:title "Manage ECM")  
	(navbar :active "Manage")
	(when manager?
	  
	  (when (ignore-errors (maxclaims/data-entity/app-user:app-user-administrator-p (get-app-user)))
	  
	  (view-page (make-instance 
		      'maxclaims/ecm-description::manage-viewer) 
		     :active-tab (or active-tab :first)
		     :navbar nil
		     :offsets (http-parameters-as-alist "offset")))))
    ))

