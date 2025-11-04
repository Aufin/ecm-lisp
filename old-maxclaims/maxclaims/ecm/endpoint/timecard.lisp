(defpackage :ecm/endpoint/timecard
  (:use :cl)
  (:import-from :ecm/user)
  (:import-from :ecm/request-context)
  (:import-from :ecm/endpoint
		#:define-endpoint)
  (:import-from :ecm/ui/timecard
		#:timecard-page)
  (:import-from :ecm/json #:getjso)
  (:import-from :ecm/entity/timecard))
(in-package :ecm/endpoint/timecard)

(ecm/endpoint:define-endpoint claim-create-interim
    "ecm/claim/(\\d+)/interim/create$")

(defun claim-create-interim/get
    (claim-id
     &aux (claim-id (parse-integer claim-id)))
  (ecm/request-context:with-request-context ()
    (ecm/ui/timecard:create-or-edit-interim-page
     :claim-id claim-id :edit t :create t)))

(defun claim-create-interim/post
    (claim-id
     &aux (claim-id (parse-integer claim-id)))
  (ecm/request-context:with-request-context ()
    (handler-case
	(progn (ecm/entity/timecard:create-claim-interim
		claim-id
		(ecm/hunchentoot:parameter-or-nil "date"))
	       (ecm/hunchentoot:redirect
		(format nil "/ecm/claim/~A" claim-id)))
      (error (c)	
	(ecm/ui/timecard:create-or-edit-interim-page
	 :claim-id claim-id :error c :edit t :create t)))))
  
(ecm/endpoint:define-endpoint interim
    "ecm/interim/(\\d+)$")

(defun interim/get (interim-id
		    &aux (interim-id (parse-integer interim-id)))
  (ecm/request-context:with-request-context ()
    (ecm/ui/timecard:interim-page interim-id)))

(ecm/endpoint:define-endpoint interim-edit
    "ecm/interim/(\\d+)/edit$")

(defun interim-edit/get (interim-id
		    &aux (interim-id (parse-integer interim-id)))
  (ecm/request-context:with-request-context ()
    (ecm/ui/timecard:create-or-edit-interim-page :interim-id interim-id
						 :create nil)))

(defun interim-edit/post
    (interim-id
     &aux (interim-id (parse-integer interim-id)))
  (ecm/request-context:with-request-context ()
    (handler-case
	(progn (ecm/entity/timecard:update-interim
		interim-id
		(ecm/hunchentoot:parameter-or-nil "date"))
	       (ecm/hunchentoot:redirect
		(format nil "/ecm/interim/~A" interim-id)))
      (error (c)
	(ecm/ui/timecard:create-or-edit-interim-page
	 :interim-id interim-id :error c :edit t)))))


(defmacro timecard-let ((&key timecard-id) &body body)
  `(let ((user-id (param-or-nil "user-id"
				#'parse-number:parse-number))
	 (claim-id (param-or-nil "claim-id"
				#'parse-number:parse-number))
	 (date (param-or-nil "date"))
	 (notes (param-or-nil "notes"))
	 (bill (param-or-nil "billable"
			     #'parse-number:parse-number))
	 (unbill (param-or-nil "unbillable"
			       #'parse-number:parse-number))
	 (mileage (param-or-nil  "mileage"
				 #'parse-number:parse-number))
	 (disbursement (param-or-nil
			"disbursement"
			#'parse-number:parse-number))
	 (attachment (param-or-nil
		      "ecmAttachment"
		      #'parse-number:parse-number))
	 (timecard-id (or ,timecard-id
			  (param-or-nil
			   "timecard-id"
			   #'parse-number:parse-number))))
     ,@body))
  
(define-endpoint timecard
    "ecm/timecard/(\\d+)/?(edit)?")

(defun timecard/get (timecard-id thing
		     &aux (timecard-id (parse-integer timecard-id)))

  (ecm/user:with-user ()
    (let* ((timecard (ecm/entity/timecard:find-timecard timecard-id))
	   (claim-id (getjso "claim_id" timecard)))
      (ecm/ui/timecard:timecard-page
       :edit (equalp thing "edit")
       :claim-id claim-id
       :timecard-id timecard-id
       :title (if (equalp thing "edit")
		  "Update Timecard"
		  "Timecard")
       :date (getjso "date" timecard)
       :notes (getjso "notes" timecard)
       :billable-hours (getjso "billable_hours" timecard)
       :unbillable-hours (getjso "unbillable_hours" timecard)
       :mileage (getjso "mileage" timecard)
       :disbursement (getjso "disbursement" timecard)
       :user-id (ecm/json:getjso* "user._id" timecard)
       :attachment-id (or (ecm/json:getjso* "attachment._id" timecard)
			  #+(or)(error "No attachment ~A" timecard))))))

(defun timecard/post (timecard-id thing
		      &aux (timecard-id (parse-integer timecard-id)))
  (declare (ignore thing))
  (timecard-let (:timecard-id timecard-id)
    (Handler-case
	(ecm/user:with-user ()	  
	  (unless (and date (or bill unbill))
	    (error "Timecard must have hours, either Billable or Unbillable, and be set for a Date and Time."))
	  (ecm/entity/timecard:update-timecard
	   timecard-id
	   :claim-id claim-id
	   :user-id user-id
	   :date date
	   :attachment-id attachment
	   :notes notes
	   :billable-hours bill
	   :unbillable-hours unbill
	   :mileage mileage
	   :disbursement disbursement)
	  (hunchentoot:redirect
	   (concatenate 'string "/ecm/claim/"
			(princ-to-string claim-id)
			"?timecard=" (princ-to-string timecard-id))))
      (error (c)
	(ecm/ui/timecard:timecard-page
	 :claim-id claim-id
	 :timecard-id timecard-id
	 :error c
	 :title "Update Timecard"
	 :date date
	 :notes notes
	 :billable-hours bill
	 :unbillable-hours unbill
	 :mileage mileage
	 :disbursement disbursement
	 :attachment-id attachment)))))
  
       


(define-endpoint claim-timecard
    "ecm/claim/(\\d+)/timecard/(.*)")


(defun claim-timecard/get (claim-id thing)
  (let ((claim-id (parse-integer claim-id)))
    (ecase (intern thing :ecm/endpoint/timecard)
      (|create| 
       (ecm/ui/timecard:create-timecard-page claim-id)))))

(defun param-or-nil (name &optional (identity #'identity))
  (let ((value (hunchentoot:post-parameter name)))
    (when (and value (not (string= value "")))
      (funcall identity value))))

(defun claim-timecard/post (claim-id thing)
  (ecm/user:with-user ()
    (let ((claim-id (parse-integer claim-id)))
      (ecase (intern thing :ecm/endpoint/timecard)
	(|create|
	 (let ((user-id (ecm/user:user-id))
	       (date (param-or-nil "date"))
	       (notes (param-or-nil "notes"))
	       (bill (param-or-nil "billable"
				   #'parse-number:parse-number))
	       (unbill (param-or-nil "unbillable"
				     #'parse-number:parse-number))
	       (mileage (param-or-nil  "mileage"
				       #'parse-number:parse-number))
	       (disbursement (param-or-nil
			      "disbursement"
			      #'parse-number:parse-number))
	       (attachment (param-or-nil
			    "ecmAttachment"
			    #'parse-number:parse-number)))
	   (handler-case
	       (progn
		 (unless (and date (or bill unbill))
		   (error "Timecard must have hours, either Billable or Unbillable, and be set for a Date and Time."))
		 (let ((timecard-id
			(ecm/entity/timecard:create-timecard
			 :claim-id claim-id
			 :user-id user-id
			 :date date
			 :notes notes
			 :billable-hours bill
			 :unbillable-hours unbill
			 :mileage mileage
			 :disbursement disbursement
			 :attachment-id attachment)))
		   (hunchentoot:redirect
		    (concatenate 'string "/ecm/claim/"
				 (princ-to-string claim-id)
				 "?timecard=" (princ-to-string timecard-id)
				 ))))
	     (error (c)
	       (ecm/ui/timecard:create-timecard-page
		claim-id :error c
		:date date
		:notes notes
		:billable-hours bill
		:unbillable-hours unbill
		:mileage mileage
		:disbursement disbursement
		:attachment-id attachment)))))))))
  
  




