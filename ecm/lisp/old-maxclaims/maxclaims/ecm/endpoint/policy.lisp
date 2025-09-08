(defpackage :ecm/endpoint/policy
  (:use :cl)
  (:import-from :ecm/user)
  (:import-from :ecm/entity/policy)
  (:import-from :ecm/entity/corpus)
  (:import-from :ecm/request-context
		#:with-request-context)
  (:import-from :ecm/endpoint
		#:define-endpoint))
(in-package :ecm/endpoint/policy)

(define-endpoint policy-search "/ecm/policy/search")

(defun policy-search/get ()
  (with-request-context ()
    (let* ((term (ecm/hunchentoot:parameter "term"))
	   (policies (ecm/entity/policy:search-for-policy term :limit 25)))
      (loop for p in policies
	 :do (setf (ecm/json:getjso "insured_name" p)
		   (ecm/entity/corpus:corpus-name-as-string
		    (ecm/json:getjso "insured" p))))
    (ecm/request-context:send-json-response
     policies))))

(define-endpoint policy-inline "/ecm/policy/(\\d+)/inline$")

(defun policy-inline/get (policy-id &aux (policy-id (parse-integer policy-id)))
  (with-request-context ()
    (let ((policy (ecm/entity/policy:find-policy policy-id)))
    (with-output-to-string (ecm/ml:*sexpml-output*)
      (ecm/ui/policy:<policy> policy)))))


  
			    
