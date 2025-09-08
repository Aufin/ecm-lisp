(defpackage :ecm/entity/timecard
  (:use :cl)
  (:import-from :ecm/json
		#:getjso)
  (:import-from :ecm/user)
  (:export #:create-timecard
	   #:find-timecard
	   #:update-timecard
	   #:find-claim-continual-timecard
	   #:find-interim
	   #:update-interim
	   #:user-can-edit-timecard-p
	   #:create-claim-interim))
(in-package :ecm/entity/timecard)

(defun user-can-edit-timecard-p (user timecard-id)
  (let ((_id (ecm/user:user-id user)))
    (postmodern:query
     (:select (:= 'app-user-id _id)
	      :from 'timecard
	      :where (:= 'timecard-id timecard-id))
     :single)))
  

(defun find-timecard (timecard-id)
  (ecm/json:read-json-from-string
   (postmodern:query
   (:select (:jsi.timecard timecard-id))
   :single)))

(defun find-timelog (claim-id)
  (ecm/database:query/return-jso
    (:select (:jsi.claim-timelog claim-id))))

(defun find-interim (interim-id)
  (ecm/database:query/return-jso
   (:select (:jsi.timecard-interim-crux interim-id))))

(defun find-claim-continual-timecard (claim-id)
  (ecm/database:query/return-jso
    (:select (:jsi.claim-continual-timecard claim-id))))

(defun create-claim-interim (claim-id time)
  (ecm/database:query/return-jso
    (:insert-into
     'timecard-interim
     :set 'claim-id claim-id
     'date time
     :returning (:jsi.timecard_interim 'timecard-interim))))

(defun update-interim (interim-id time)
  (ecm/database:query/return-jso
    (:update
     'timecard-interim
     :set 'date time
     :where (:= 'timecard_interim_id interim-id)
     :returning (:jsi.timecard_interim 'timecard-interim))))
	    
(defun create-timecard (&key claim-id user-id
			  date
			  notes
			  billable-hours
			  unbillable-hours
			  mileage
			  disbursement
			  attachment-id)

  (postmodern:query
   (:insert-into
    'timecard
    :set
    'claim-id claim-id
    'app-user-id user-id
    'date date
    'notes (or notes :null)
    'minutes (or billable-hours 0)
    'unbillable-hours (or unbillable-hours 0)
    'mileage-km (or mileage 0)
    'disbursements (or disbursement 0)
    'attachment-id (or attachment-id :NULL)
    :returning 'timecard-id)
   :single))

(defun update-timecard (timecard-id
			&key claim-id
			  user-id
			  date
			  notes
			  billable-hours
			  unbillable-hours
			  mileage
			  disbursement
			  attachment-id)

  (postmodern:query
   (:update 'timecard
    :set
    'claim-id claim-id
    'app-user-id user-id
    'date date
    'notes (or notes :null)
    'minutes (or billable-hours 0)
    'unbillable-hours (or unbillable-hours 0)
    'mileage-km (or mileage 0)
    'disbursements (or disbursement 0)
    'attachment-id (or attachment-id :NULL)
    :where (:= 'timecard-id timecard-id))
   :single))
    
		 
   
