(defpackage :maxclaims/ucw/application
  (:use :cl)
  (:import-from :ucw-core
		#:find-session)
  (:import-from :ucw-standard
		#:request-context-class)
  (:export 
   #:*ecm-application*
   #:ecm-application))
  
(in-package :maxclaims/ucw/application)

(setf ucw-core:*REQUEST-CONTENT-LENGTH-LIMIT* 
      (* 1024 1024 1024))

(progn 
  (defclass ecm-application (ucw:standard-application
			     ucw:cookie-session-application-mixin
			     ucw:static-roots-application-mixin)
    ()
    (:default-initargs 
     :url-prefix "/ecm/"))
  (defparameter *ecm-application* (make-instance 'ecm-application)))


