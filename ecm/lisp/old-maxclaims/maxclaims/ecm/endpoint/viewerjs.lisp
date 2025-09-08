(defpackage :ecm/endpoint/viewerjs
  (:use :cl)
  (:documentation
   "http://viewerjs.org/examples/")
  (:import-from :ecm/user)
  (:import-from :ecm/endpoint
		#:define-endpoint)
  (:import-from :ecm/json #:getjso)
  (:import-from :ecm/request-context))
(in-package :ecm/endpoint/viewerjs)

(defvar *path* (merge-pathnames
		"ui/ViewerJS/"
		(asdf:system-source-directory :ecm)))
(defparameter *hunchentoot-viewerjs-static-handler*
  (hunchentoot:create-folder-dispatcher-and-handler 
   "/ecm/viewerjs/" *path*))

(define-endpoint index "/ecm/viewerjs/")

(defun index/get ()
  (ecm/hunchentoot:handle-static-file
   (merge-pathnames "index.html" *path*)))

(pushnew *hunchentoot-viewerjs-static-handler* hunchentoot::*dispatch-table*)


