(defpackage :ecm/user
  (:use :cl)
  (:import-from :hunchentoot)
  (:import-from :ecm/local-time)
  (:import-from :ecm/json)
  (:import-from :maxclaims)
  (:export #:user
	   #:user-timezone
	   #:user-local-time-timezone
	   #:user-id
     #:username
     #:user-name
     #:find-user-by-id
     #:bind-user
	   #:user-person-id
	   #:user-password
	   #:user-database-role
	   #:user-read-only-p
	   #:user-can-edit-p
	   #:user-is-administrator-p
	   #:user-can-edit-p
	   #:call-with-user
	   #:with-user
	   #:no-user-available))
(in-package :ecm/user)

(defvar *user* nil)

(defun user ()
  *user*)

(defun find-user-by-id (uid)
  (maxclaims::with-adb
    (maxclaims::select-only-n-objects
     1 'maxclaims::app-user
     :where `(:= ,uid app-user-id))))

(defun bind-user (user fun)
  (let ((*user* user))
    (maxclaims::call-with-app-user
     *user* fun)))

(defvar *user-timezone* nil)

(defun db/user-timezone (&optional (user (user)))
  (let ((tz (ecm/json:null->nil
	     (postmodern:query
	      (concatenate
	       'string
	       "SELECT app_user.data->'timezone' FROM app_user WHERE app_user_id = "
	       (princ-to-string (user-id user)))
	      :single))))
    (postmodern:query
     "SELECT \"user\".update_contract_permissions($1)"
     (user-id user))
    (when tz (ecm/json:read-json-from-string tz))))

(defun (setf db/user-timezone) (tz &optional (user (user)))
  (prog1 
      (ecm/json:null->nil
       (ecm/json:read-json-from-string
	(postmodern:query
	 (:with (:as 'new-data
		 (:select (:as (:pongo.json-concat
				(:pongo.json-remove 'data "timezone")
				(:json-build-object
				 "timezone" (:type (ecm/json:write-json-to-string tz)
						   json)))
			       'data)
			  :from 'app-user
			  :where (:= 'app-user-id (user-id user))))
		
     (:update
      'app-user
      :set 'data (:select 'data :from 'new-data)     
      :where (:= 'app-user-id (user-id user))
      :returning 'app-user.data))
     :single)))
    (postmodern:query
     "SELECT \"user\".update_contract_permissions($1)"
     (user-id user))))

(defun default-timezone ()
  (ecm/json:jso "location" "America/Vancouver"
		"timezone" "PST"
		"gmt_offset" "-08:00"))

(defun session-timezone ()
  (hunchentoot:session-value 'timezone))
  
(defun user-timezone (&optional (user (user)))
  ;(break "user-timezone ~A ~A" (hunchentoot:session-value 'timezone) *user-timezone*)
  (multiple-value-bind (tz bound? session? db?)
      (if *user-timezone*
	  (values *user-timezone* t)
	  (let ((tz (ignore-errors (hunchentoot:session-value 'timezone))))
	    (if tz
		(values tz nil t)		
		(when user
		  (let ((tz (db/user-timezone user)))
		    (when tz (values tz nil nil t)))))))
    (if tz (values tz bound? session? db?)
	(values (default-timezone) nil nil nil))))

(defun (setf user-timezone) (tz &optional (user (user)))
  (when tz (setf (hunchentoot:session-value 'timezone) tz))
  (unless tz
    (setf tz :null)
    (hunchentoot:delete-session-value 'timezone))
  (setf (db/user-timezone user) tz))

(defun user-local-time-timezone (&optional (user (user)))
  (ecm/local-time:find-timezone-by-location-name
   (ecm/json:getjso "location" (user-timezone user))))

(defun username  (&optional (user (user))) (maxclaims::app-user.username user))
(defun user-name (&optional (user (user))) (username user))
(defun user-id (&optional (user (user))) (maxclaims::app-user.app-user-id user))
(defun user-person-id (&optional (user (user)))
  (maxclaims::app-user.person-id user))

(defun user-password (&optional (user (user)))
  (maxclaims::app-user.password user))

(defun user-database-role (&optional (user (user)))
  (concatenate 'string "mu_" (princ-to-string (user-id user))))

(defun user-read-only-p (&optional (user (user)))
  (postmodern:query
   (:SELECT (:app-user-is-read-only 'app-user)
	    :FROM 'app-user
	    :WHERE (:= 'app-user-id (user-id user)))
   :single))

(defun user-is-administrator-p (&optional (user (user)))
  (postmodern:query
   (:SELECT (:app-user-is-administrator 'app-user)
	    :FROM 'app-user
	    :WHERE (:= 'app-user-id (user-id user)))
   :single))

(defun %user-can-edit-p (user)
  (postmodern::query (:select 'can_edit :from 'app_user
			       :where (:= 'app_user_id (user-id user)))
		      :single))

(defun user-can-edit-p (&key (user (user))
			  type 
			  &allow-other-keys)
  (or (user-is-administrator-p user)
      (and (%user-can-edit-p user)
	    (not (string-equal "timecard" type))
	    (not (string-equal "contract" type)))))

(define-condition no-user-available (error)
  ((condition :initarg condition :reader no-user-available-condition
	      :initform "None"))
  (:report (lambda (condition stream)
             (format stream "No User available : ~A"
		     (no-user-available-condition condition)))))

(defun call-with-user (function)
  (let ((*user* (hunchentoot:session-value :app-user)))
    (if *user*
        (maxclaims::call-with-app-user
         *user*         
         (lambda ()
           (maxclaims::with-udb
             (funcall function *user*))))
        (error 'no-user-available))))

(defmacro with-user ((&optional user) &body body)
  (let ((user (or user (gensym))))
    `(call-with-user (lambda (,user)
		       (declare (ignorable ,user))
		                   ,@body))))
