(defpackage :maxclaims/entry-point/message
  (:use :cl)
  (:import-from :maxclaims/web-display/html-page
		#:call-with-html-page)
  (:import-from :maxclaims/entry-point/create
		#:create-page-handler)
  (:import-from :maxclaims/hunchentoot
		#:define-easy-handler))
  

(in-package :maxclaims/entry-point/message)

(define-easy-handler (|message| :uri "/ecm/message") 
    ()
  (create-page-handler))
  
