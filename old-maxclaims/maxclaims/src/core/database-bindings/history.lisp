(in-package :maxclaims)

(defclass history.hstore-history ()
  ((hstore-history-id :primary-key t)
   (modification)
   (row-type)
   (row-id)
   (user-role)
   (modification-time)
   (history)
   (previous))
   (:metaclass described-db-access-class))
  

(defclass history ()
  ((history-id :primary-key t)
   (object-type)
   (object-id)
   (app-user-id)
   (app-user :column app-user-id
	     :references app-user)
   (modification-time)
   (history-data))
  (:metaclass described-db-access-class))