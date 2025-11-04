(defpackage :ecm/endpoint/redirect
  (:use :cl)
  (:import-from :ecm/hunchentoot)
  (:import-from :ecm/endpoint
		#:define-endpoint))
(in-package :ecm/endpoint/redirect)

(define-endpoint index-ucw "maxwell/index.ucw")

(defun index-ucw/get ()
  (ecm/hunchentoot:redirect "/ecm/index"
			    :protocol :https))
