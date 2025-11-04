(defpackage :maxclaims/entry-point/todo
  (:use :cl)
  (:import-from :maxclaims/hunchentoot
		#:define-easy-handler))
  

(in-package :maxclaims/entry-point/todo)

(define-easy-handler (|todo| :uri "/ecm/todo")
    ()
  (<:as-is 
   (alexandria:read-file-into-string 
    (merge-pathnames "agenda.html" 
		     (asdf:system-source-file :maxclaims-ecm)))))
  
