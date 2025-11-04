(in-package #:maxclaims)

(defclass diary-entry ()
  ((diary-entry-id :primary-key t)
   (user :references app-user
	 :column app-user-id
	 :initform $app-user)
   (claim :references claim
	  :column claim-id)
   (defered :referenced-from defer-diary-entry)
   (processed :accessor diary-entry.processedp)
   action-date
   note
   app-user-id
   claim-id)
  (:metaclass described-db-access-class))
