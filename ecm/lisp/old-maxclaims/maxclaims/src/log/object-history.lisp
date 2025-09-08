(defpackage :maxclaims/log/object-history
  (:use :cl )
  (:import-from :maxclaims
		#:with-adb
		#:with-db)		
  (:export #:select-object-history
	   #:select-object-previous-history))

(in-package :maxclaims/log/object-history)

(defun select-object-history (object-type object-pkey)
  (with-adb
    (postmodern:query 
     "SELECT modification,
             modification_time,
             app_user.username,
             history,
             previous
      FROM history.hstore_history 
      LEFT JOIN app_user 
      ON app_user_id = substring (user_role FROM 1 + position ('_' IN user_role))::integer

 
      WHERE NOT user_role like 'maxclaims' AND 
      row_type = $1 
      AND row_id = $2
      ORDER BY hstore_history_id DESC LIMIT 100"
     object-type
     object-pkey
     :str-alists)))

(defun select-object-previous-history (object-type object-pkey)
  (with-adb
    (postmodern:query 
     (:select 'h.* 
	      'a.username
	      :from (:as 'history 'h)
      :left-join (:as 'app-user 'a)
      :on (:= 'a.app-user-id
	      'h.app-user-id)
      :where (:and (:ilike  (string object-type) 'object-type)
		   (:= object-pkey 'object-id)))
          :str-alists)))

  

