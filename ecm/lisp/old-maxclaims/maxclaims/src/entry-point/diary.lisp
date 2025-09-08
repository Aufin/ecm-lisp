(defpackage :maxclaims/entry-point/diary
  (:use :cl)
  (:import-from :maxclaims/hunchentoot
		#:define-easy-handler)
  (:import-from :maxclaims/entry-point/toplevel
		#:http-query-string
		#:http-parameters-as-alist)
  (:import-from :maxclaims/web-display/html-page
		#:get-app-user
		#:with-user-html-page)
  (:import-from :maxclaims 
		#:with-udb)
  (:import-from :maxclaims/web-display/view
		#:view-page-tabs))
  
(in-package :maxclaims/entry-point/diary)

(define-easy-handler (diary :uri "/ecm/diary") 
   
    ()
  (with-user-html-page (:title "Diary")
    (with-udb 
      (view-page-tabs (get-app-user) 
		      :view-tab :diary
		      :offsets (http-parameters-as-alist "offset")))))
  
