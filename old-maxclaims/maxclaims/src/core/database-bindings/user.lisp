(in-package :maxclaims)

(defclass app-user ()
  ((app-user-id :primary-key t)
   (contracts :referenced-from app-user-contract)
   (claims :referenced-from app-user-claim)
   (authority :referenced-from app-user-authority)
   (agency :referenced-from app-user-agency)
   (syndicates :referenced-from app-user-syndicate)
   username 
   password 
   (person :column person-id :references person)
   person-id 
   admin
   can-edit
   login
   (log :transient t))
  (:metaclass described-db-access-class))

(defclass app-user-claim ()
  ((app-user-id :primary-key t)
   (claim-id :primary-key t)
   (user :column app-user-id :references app-user)
   (claim :column claim-id :references claim :on claim-id)
   (access))
  (:metaclass described-db-access-class))

(defclass app-user-authority ()
  ((app-user-id :primary-key t)
   (user :column app-user-id :references app-user)
   (claim-incurred-limit))
  (:metaclass described-db-access-class))

(defclass app-user-contract ()
  ((app-user-app-resource-id :primary-key t)
   app-user-id 
   contract-id
   (contract :column contract-id
	     :references contract)
   (user :column app-user-id 
	 :references app-user
	 :on app-user-id))
  (:metaclass described-db-access-class))

(defclass app-user-agency ()
  ((app-user-agency-id :primary-key t)
   app-user-id agency-id
   (agency :column agency-id :references person :on person-id)
   (user :column app-user-id 
	 :references app-user
	 :on app-user-id))
  (:metaclass described-db-access-class))

(defclass app-user-syndicate()
  ((app-user-syndicate-id :primary-key t)
   app-user-id syndicate-id
   (syndicate :column syndicate-id :references person :on person-id)
   (user :column app-user-id
	 :references app-user
	 :on app-user-id))
  (:metaclass described-db-access-class))
