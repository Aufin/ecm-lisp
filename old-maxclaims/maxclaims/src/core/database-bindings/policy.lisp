(in-package :maxclaims)

(defclass policy ()
  ((policy-id :primary-key t)
   agent-id 
   insured-id
   (insured :column insured-id 
	    :references person
	    :accessor policy.insured)
   (agent :column agent-id
	  :references person)
   (company :column company-id
	    :references person)
   (sub-agent :column agency-office-id
	      :references person)
   (branch :column branch-id
	   :references person)
   (underwriter :column underwriter-id
		:references person)
   (risks :referenced-from risk
	  :on policy-id)
   (details :referenced-from policy-detail :on policy-id
	    :accessor policy.details)
   effective-date
   expiry-date
   policy-number 
   deductible
   company-id
   underwriter-id
   branch-id
   agency-office-id)
  (:metaclass described-db-access-class))


(defmethod find-risks ((policy policy))
  (select-objects 'risk
		  :where `(:= policy-id ,(policy.policy-id policy))))

(defun find-policy-claims (policy)
  (alexandria:mappend (lambda (risk) (ignore-errors (risk.claims risk)))
	  (policy.risks policy)))

(export '(policy-detail))

(defclass policy-detail ()
  ((policy-id :primary-key t)
   (key :initarg :key
	:primary-key t)
   (value :initarg :value))
  (:metaclass described-db-access-class))

(defclass policy-detail-key ()
  ((text :primary-key t))
  (:metaclass described-db-access-class))
