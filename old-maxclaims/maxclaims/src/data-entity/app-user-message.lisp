(defpackage :maxclaims/data-entity/app-user-message
  (:use :cl)
  (:import-from :rofl)
  (:import-from :maxclaims
		#:app-user)
  (:export 
   #:app-user-message
   #:app-user-message.app-user-message-id
   #:app-user-message.parent-message
   #:app-user-message.from
   #:app-user-message.to
   #:app-user-message.subject
   #:app-user-message.body
   #:app-user-message.unread

   #:app-user-message-id
   #:parent-message-id
   #:parent-message 
   #:from
   #:from-id
   #:to
   #:to-id
   #:subject
   #:body
   #:unread
   ))

(in-package :maxclaims/data-entity/app-user-message)

(defclass app-user-message ()
  ((app-user-message-id :primary-key t)
   parent-message-id
   (parent-message 
    :column  parent-message-id
    :references app-user-message)
   (from :column from-id 
	 :references app-user)
   from-id
   (to :column to-id 
       :references app-user)
   to-id
   subject
   body
   unread)
  (:metaclass rofl:standard-db-access-class))
