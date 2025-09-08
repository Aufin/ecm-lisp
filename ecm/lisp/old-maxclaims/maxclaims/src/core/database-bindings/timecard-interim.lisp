(in-package :maxclaims)

(defclass timecard-interim ()
  ((timecard-interim-id :primary-key t)
   (claim-id :initarg :claim-id)
   (claim :column claim-id 
	     :references claim) 
   (date ))
  (:metaclass described-db-access-class))
