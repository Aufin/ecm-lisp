(defpackage :maxclaims/entry-point/search
  (:use :cl :maxclaims/yaclml/tags)
  (:import-from :maxclaims/hunchentoot
		#:define-easy-handler)
  (:import-from :maxclaims
		#:search-records
		#:call-with-app-user)
  (:import-from :maxclaims/web-display/search
		#:search-page))

(in-package :maxclaims/entry-point/search)

(define-easy-handler (search-handler :uri "/ecm/search") 
    (q)
  (maxclaims/web-display/search:search-page q))
