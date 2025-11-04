(defpackage :maxclaims/entry-point/history
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
  (:import-from :maxclaims/web-display/history
		#:history-page)
  (:import-from :maxclaims/hunchentoot
		#:define-easy-handler))
  
(in-package :maxclaims/entry-point/history)


(define-easy-handler (|history| :uri "/ecm/history")
    ()
  (let ((object (ignore-errors (call-with-app-user 
				(get-app-user) 
				#'select-object-from-request))))
    
    (history-page object)
))

