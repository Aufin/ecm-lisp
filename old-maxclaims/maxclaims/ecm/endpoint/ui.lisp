(defpackage :ecm/endpoint/ui
  (:use :cl)
  (:import-from :ecm/user
		#:user
		#:user-id)
  (:import-from :ecm/endpoint
		#:define-endpoint)
  (:import-from :ecm/json #:getjso)
  (:import-from :ecm/entity/ui)
  (:import-from :ecm/request-context))
(in-package :ecm/endpoint/ui)

(ecm/endpoint:define-endpoint claim-ui
    "ecm/ui/claim/(\\d+)$")

(defun claim-ui/post (claim-id
		      &aux (claim-id (parse-integer claim-id)))
  (ecm/request-context:with-request-context ()
    (let ((ui (ecm/hunchentoot:get-post-body-as-json)))
      (setf (ecm/entity/ui:user-claim-ui claim-id) ui)
     ; (break "~A" (multiple-value-list (ecm/user:user-timezone)))
      (ecm/request-context:send-json-response
       (ecm/entity/ui:user-claim-ui claim-id)))))
