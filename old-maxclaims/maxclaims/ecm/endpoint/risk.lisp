(defpackage :ecm/endpoint/risk
  (:use :cl)
  (:import-from :ecm/user)
  (:import-from :ecm/request-context
		#:with-request-context)
  (:import-from :ecm/hunchentoot
		#:parameter-or-nil)
  (:import-from :ecm/endpoint
		#:define-endpoint)
  (:import-from :ecm/endpoint/policy)
  (:import-from :ecm/ui/risk
		#:risk-page)
  (:import-from :ecm/json #:getjso)
  (:import-from :ecm/entity/risk
		#:find-risk-detail
		#:update-risk)
  (:import-from :ecm/entity/claim
		#:find-claim))
(in-package :ecm/endpoint/risk)

(define-endpoint inline-edit-risk "/ecm/risk/(\\d+)/edit/inline$")

(defun inline-edit-risk/get (risk-id &aux (risk-id (parse-integer risk-id)))
  (with-request-context ()
    (risk-page (find-risk-detail risk-id)
	       :edit t :inline t)))

(defun inline-edit-risk/post (risk-id &aux (risk-id (parse-integer risk-id)))
  (with-request-context
   ()
   (let ((risk-type (parameter-or-nil "risk-type"))
	       (policy-id (parameter-or-nil "policy-id"))
	       (number (parameter-or-nil "number"))
	       (contract-id (parameter-or-nil "contract-id"))
	       (risk-code (parameter-or-nil "risk-code")))
     (handler-case
	       (let ((risk (update-risk risk-id
				                          :risk-type risk-type
				                          :policy-id policy-id
                                  :risk-number number
				                          :contract-id contract-id
				                          :risk-code risk-code)))
	    
	         (ecm/ui/risk::inline-post-page))
       (error (c)
	            (error c))))))

(define-endpoint claim-risk "/ecm/claim/(\\d+)/risk$")

(defun claim-risk/get (claim-id)
  (with-request-context ()
    (with-output-to-string (ecm/ml:*sexpml-output*)
      (ecm/ui/risk:<claim-risk> (find-claim (parse-integer claim-id))))))

(define-endpoint risk "/ecm/risk/(\\d+)$")

(defun risk/get (risk-id &aux (risk-id (parse-integer risk-id)))
  (with-request-context ()
    (risk-page (find-risk-detail risk-id))))

(define-endpoint edit-risk "/ecm/risk/(\\d+)/edit$")

(defun edit-risk/get (risk-id &aux (risk-id (parse-integer risk-id)))
  (with-request-context ()
    (risk-page (find-risk-detail risk-id) :edit t)))

(defun edit-risk/post (risk-id &aux (risk-id (parse-integer risk-id)))
  (with-request-context
   ()
   (let ((risk-type (parameter-or-nil "risk-type"))
	       (policy-id (parameter-or-nil "policy-id"))
	       (contract-id (parameter-or-nil "contract-id"))
	       (number (parameter-or-nil "number"))
	       (risk-code (parameter-or-nil "risk-code")))
     (handler-case
	       (risk-page (update-risk risk-id
				                         :risk-type risk-type
                                 :risk-number number
				                         :policy-id policy-id
				                         :contract-id contract-id
				                         :risk-code risk-code))
       (error (c)
	            (error c)))))) ;;(error "~A" (list risk-type policy-id contract-id risk-code)))))
