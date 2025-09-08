(in-package :maxclaims)

(defclass contract ()
  ((contract-id :primary-key t)
   contract-number 
   effective-date
   expiry-date
   agency-id
   syndicate-id
   london-broker-id
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
