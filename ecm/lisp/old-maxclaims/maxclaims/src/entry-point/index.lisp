(defpackage :maxclaims/entry-point/index
  (:use :cl)
  (:import-from :maxclaims/hunchentoot
		#:define-easy-handler)
  (:import-from :maxclaims/entry-point/toplevel
		#:http-query-string
		#:http-parameters-as-alist)
  (:import-from :maxclaims/web-display/index
		#:index-page))
  
(in-package :maxclaims/entry-point/index)

(define-easy-handler (index :uri "/ecm/index") 
   
    ()
  (index-page 
   :http-query-string (http-query-string)
   :offsets (http-parameters-as-alist "offset")))

(define-easy-handler (slash :uri "/") 
   
    ()
  (maxclaims/hunchentoot:redirect "/ecm/index"))
  
