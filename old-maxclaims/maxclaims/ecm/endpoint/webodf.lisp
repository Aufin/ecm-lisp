(defpackage :ecm/endpoint/webodf
  (:use :cl)
  (:import-from :ecm/user)
  (:import-from :ecm/endpoint
		#:define-endpoint)
  (:import-from :ecm/json #:getjso)
  (:import-from :ecm/request-context))
(in-package :ecm/endpoint/webodf)

(defparameter *hunchentoot-webodf-static-handler*
  (hunchentoot:create-folder-dispatcher-and-handler 
   "/ecm/webodf/" (merge-pathnames "ui/webodf/public-html/"
				(asdf:system-source-directory :ecm))))

(pushnew *hunchentoot-webodf-static-handler* hunchentoot::*dispatch-table*)
