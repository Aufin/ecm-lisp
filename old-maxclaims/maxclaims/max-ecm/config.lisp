(cl:defpackage :max-ecm/config
  (:use :cl)
  (:import-from :max-ecm/database-interface
		#:*database-connection-parameters*)
  (:import-from :max-ecm/url-scheme-variables
		#:*server-public-url*)
  (:import-from :max-ecm/mail
		#:*mail-authentication*))
(cl:in-package :max-ecm/config)

(defparameter *mail-authentication*
  '("no" "way"))

(defparameter *database-connection-parameters* 
  '("maxclaims" "maxclaims" "" "localhost" :port 5433))

(defparameter *server-public-url* "")


