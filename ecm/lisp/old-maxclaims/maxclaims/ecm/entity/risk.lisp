(defpackage :ecm/entity/risk
  (:use :cl)
  (:import-from :ecm/json
		#:getjso)
  (:import-from :ecm/user)
  (:export #:find-risk-detail
	   #:risk-types
	   #:active-risk-codes
	   #:update-risk
	   #:clone-risk))
(in-package :ecm/entity/risk)

(defun find-risk-detail (risk-id)
  (ecm/database:query/return-jso
    (:select (:jsi.risk-crux risk-id))))

(defun risk-types ()
  (postmodern:query "SELECT type_name FROM risk_type ORDER BY type_name"
		    :column))

(defun active-risk-codes ()
  (ecm/database:query/return-jso
    (:select (:jsi.active-risk-codes))))

(defun clone-risk (risk-id)
  (let* ((risk (find-risk-detail risk-id))
	 (type (getjso "risk_type" risk))
	 (policy-id (ecm/json:getjso* "policy._id" risk))
	 (contract-id (ecm/json:getjso* "contract._id" risk))
	 (risk-code (ecm/json:getjso* "risk_code.code" risk)))
    (insert-risk :risk-type type
		 :policy-id policy-id
		 :contract-id contract-id
		 :risk-code risk-code)))

(defun insert-risk (&rest args
		    &key risk-type policy-id risk-number
		      contract-id (risk-code nil risk-code-provided?))
  (declare (ignore  risk-type policy-id risk-number
		    contract-id risk-code))
  (let ((set (loop :for (k v) :on args :by #'cddr
		:nconc (if (not (eql :risk-code k))
			   (when v (list (if (eql :risk-type k)
					     :risk-type-name
					     k)
					 v))
			   (when risk-code-provided?
			     (list k (or v :null)))))))
    
    (ecm/database:query/return-jso
      (s-sql:sql-compile `(:insert-into risk :set ,@set
					:returning (:jsi.risk-crux risk))))))
  
(defun update-risk
    (risk-id &rest args
             &key risk-type policy-id risk-number
		     contract-id (risk-code nil risk-code-provided?))
  (declare (ignore  risk-type policy-id risk-number
		            contract-id risk-code))
  (let ((set (loop :for (k v) :on args :by #'cddr
		           :nconc (if (not (eql :risk-code k))
			                  (when v (list (if (eql :risk-type k)
					                            :risk-type-name
					                          k)
					                        v))
			                (when risk-code-provided?
			                  (list k (or v :null)))))))
    (warn "Setting risk to ~A" set)
    (ecm/database:query/return-jso
     (s-sql:sql-compile `(:update risk
                                  :set ,@set :where (:= risk-id ,risk-id) :returning (:jsi.risk-crux risk))))))
