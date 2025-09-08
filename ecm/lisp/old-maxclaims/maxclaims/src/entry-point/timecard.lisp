(defpackage :maxclaims/entry-point/timecard
  (:use :cl)
  (:import-from :maxclaims/web-display/timecard
		#:timecard-page)
  (:import-from :maxclaims/hunchentoot
		#:define-easy-handler))
  

(in-package :maxclaims/entry-point/timecard)

(define-easy-handler (|timecard| :uri "/ecm/timecard")
    ((claim-id :real-name "timecard[claim]")
     (interim-date :real-name "timecard[interim]"))
  (timecard-page  
   :navbar t
   :claim-id claim-id
   :interim-date interim-date))
  
