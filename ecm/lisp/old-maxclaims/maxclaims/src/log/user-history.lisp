(defpackage :maxclaims/log/user-history
  (:use :cl )
  (:import-from :maxclaims
		#:with-adb
		#:select-limit*
		#:app-user
		#:app-user.app-user-id
		#:app-user-id-rolename
		#:app-user-rolename
		#:app-user.password
		#:*db-connection-parameters*
		#:with-db)		
  (:export #:select-user-history))

(in-package :maxclaims/log/user-history)

(defun select-user-history (user)
  (with-adb
    (postmodern:query 
     "SELECT * from history.hstore_history 
      WHERE user_role = $1 
      ORDER BY hstore_history_id DESC LIMIT 25"
     (app-user-id-rolename
      (typecase user
	(number user)
	(app-user (app-user.app-user-id user))))
     :plists)))

  

