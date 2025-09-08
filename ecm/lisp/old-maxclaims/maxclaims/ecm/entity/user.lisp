(defpackage :ecm/entity/user
  (:use :cl)
  (:import-from :ecm/json
		#:getjso)
  (:import-from :ecm/database)
  (:export #:find-user))
(in-package :ecm/entity/user)

(defun find-user (user-id)
  (ecm/json:read-json-from-string
   (postmodern:query
   (:select (:jsi.user-summary user-id))
   :single)))  
