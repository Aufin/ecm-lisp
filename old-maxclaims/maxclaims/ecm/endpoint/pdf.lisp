(defpackage :ecm/endpoint/pdf
  (:use :cl)
  (:import-from :ecm/user)
  (:import-from :ecm/endpoint
		#:define-endpoint)
  (:import-from :ecm/json #:getjso)
  (:import-from :ecm/request-context))
(in-package :ecm/endpoint/pdf)

(defparameter *hunchentoot-pdf-static-handler*
  (hunchentoot:create-folder-dispatcher-and-handler 
   "/ecm/pdf/" (merge-pathnames "ui/pdf/public-html/"
				(asdf:system-source-directory :ecm))))

(pushnew *hunchentoot-pdf-static-handler* hunchentoot::*dispatch-table*)



*hunchentoot-pdf-static-handler*
