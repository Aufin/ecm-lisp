(in-package :maxclaims)

(defclass app-user-message ()
  ((app-user-message-id :primary-key t)
   (parent-message-id :references app-user-message)
   (from :column from-id 
	 :references app-user)
   from-id
   (to :column to-id 
       :references app-user)
   to-id
   subject
   body
   unread)
  (:metaclass described-db-access-class))

