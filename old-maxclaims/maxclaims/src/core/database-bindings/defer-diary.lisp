(in-package :maxclaims)

(defclass defer-diary-entry ()
  ((defer-diary-entry-id :primary-key t)
   (diary-entry :references diary-entry
		:column diary-entry-id)
   (diary-entry-id :initarg :diary-entry-id)
   (defer-date))
  (:metaclass described-db-access-class))
