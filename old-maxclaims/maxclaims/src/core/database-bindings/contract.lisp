(in-package :maxclaims)

(defclass settlement-type ()
  ((name :primary-key t))
  (:metaclass described-db-access-class))

(defun settlement-types ()
  (select-objects 'settlement-type))


(defclass contract ()
  ((contract-id :primary-key t)
   contract-number 
   effective-date
   expiry-date
   agency-id
   syndicate-id
   london-broker-id
   loss-fund
   settlement-type
   (agency :column agency-id 
	   :references (person person-id))
   (syndicate :column syndicate-id 
	      :references (person person-id))
   (london-broker :column london-broker-id 
	      :references (person person-id))
   
   (risks :referenced-from risk
	  :on contract-id)
   insurance-company-id)
  (:metaclass described-db-access-class))



(defclass contract-authority ()
  ((contract-id :primary-key t)
   (contract :references contract
	  :column contract-id
	  :initarg :contract)
   (authority :initarg :authority))
  (:metaclass described-db-access-class))
