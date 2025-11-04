(defpackage :ecm/endpoint/diary
  (:use :cl)
  (:import-from :ecm/user)
  (:import-from :ecm/hunchentoot #:parameter-or-nil)
  (:import-from :ecm/endpoint
		#:define-endpoint)
  (:import-from :ecm/request-context
		#:with-request-context)
  (:import-from :ecm/json #:getjso)
  (:import-from :ecm/entity/diary))
(in-package :ecm/endpoint/diary)
  
(define-endpoint claim-create-diary-entry
    "ecm/claim/(\\d+)/diary-entry/create")

(defun claim-create-diary-entry/get (claim-id &aux (claim-id (parse-integer claim-id)))
  (with-request-context ()
         
    (ecm/ui/diary:diary-entry-page (ecm/json:jso "claim_id" claim-id)
				   :edit t :create t)))

(defun claim-create-diary-entry/post
    (claim-id &aux (claim-id (parse-integer claim-id)))
  (with-request-context ()
    (let ((schedule (parameter-or-nil "schedule"))
	  (note (parameter-or-nil "note"))
	  (user-id (parameter-or-nil "user-id"))
	  (claim-id (or (parameter-or-nil "claim-id") claim-id))
	  (processed (parameter-or-nil "processed")))
      (handler-case (let ((entry (ecm/entity/diary:insert-diary-entry
				  :schedule schedule
				  :note note
				  :user-id user-id
				  :claim-id claim-id
				  :processed processed)))         
		      (ecm/ui/diary:diary-entry-page entry))
	(error (c)
	  (ecm/ui/diary:diary-entry-page (ecm/json:jso "claim_id" claim-id
						       "schedule" schedule
						       "user_id" user-id
						       "note" note
						       "processed" processed)
					 :edit t :create t :error c))))))
	  

(define-endpoint diary-entry
    "ecm/diary-entry/(\\d+)")

(defun diary-entry/get (diary-entry-id)
  (with-request-context ()
    (let* ((diary-entry-id (parse-integer diary-entry-id))
	   (entry (ecm/entity/diary:find-diary-entry diary-entry-id)))
      (ecm/ui/diary:diary-entry-page entry))))

(define-endpoint diary-entry-edit
    "ecm/diary-entry/(\\d+)/edit")

(defun diary-entry-edit/get (diary-entry-id)
  (with-request-context ()
    (let* ((diary-entry-id (parse-integer diary-entry-id))
	   (entry (ecm/entity/diary:find-diary-entry diary-entry-id)))
      (ecm/ui/diary:diary-entry-page entry :edit t :title "Edit Diary Entry"))))

(defun diary-entry-edit/post (diary-entry-id)
  (with-request-context ()
    (let* ((diary-entry-id (parse-integer diary-entry-id))
	   (note (ecm/hunchentoot:parameter-or-nil "note"))
	   (user-id (ecm/hunchentoot:parameter-or-nil "user-id"))
	   (schedule (ecm/hunchentoot:parameter-or-nil "schedule"))
	   (deadline (ecm/hunchentoot:parameter-or-nil "deadline"))
	   (processed (ecm/hunchentoot:parameter-or-nil "processed"))
	   (claim-id (ecm/hunchentoot:parameter-or-nil
		      "claim-id"
		      :identity #'parse-integer))
	   (entry (ecm/entity/diary:update-diary-entry
		   diary-entry-id
		   :processed processed
		   :note note
		   :schedule schedule
		   :deadline deadline
		   :claim-id claim-id
		   :user-id user-id))
	   (claim-id (getjso "claim_id" entry)))
      (ecm/hunchentoot:redirect
       (concatenate 'string
		    "/ecm/claim/" (princ-to-string claim-id))))))
    
