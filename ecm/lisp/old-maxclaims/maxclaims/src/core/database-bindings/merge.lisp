(in-package :maxclaims)

(defclass merge-history ()
  ((merge-id :primary-key t)
   from-id
   into-id
   app-user-id
   modification-time)
  (:metaclass described-db-access-class))