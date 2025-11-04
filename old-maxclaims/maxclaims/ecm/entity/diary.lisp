(defpackage :ecm/entity/diary
  (:use :cl)
  (:import-from :ecm/json
		#:getjso)
  (:import-from :ecm/database)
  (:import-from :ecm/user #:user #:user-id)
  (:export #:diary-schedule-numbers
	   #:find-diary-entry
	   #:diary-users
	   #:update-diary-entry	   
	   #:insert-diary-entry))
(in-package :ecm/entity/diary)

(defun find-diary-entry (diary-entry-id)
  (let ((entry
	 (postmodern:query
	  (:select (:jsi.diary-entry 'diary-entry)
		   :FROM 'diary-entry
		   :where (:= 'diary-entry-id
			      diary-entry-id))
	  :single)))
    (when entry (ecm/json:read-json-from-string entry))))

(defun diary-users ()
  (ecm/database:query/return-jso
    (:select (:jsi.diary-users))))

(defun update-diary-entry (diary-entry-id
			   &key
			     (processed nil processed-provided?)
			     claim-id
			     note
			     schedule
			     deadline
			     user-id)
  (let* ((things `(,@(when note (list :note note))
		     ,@(when processed-provided? (list :processed processed))
		     ,@(when schedule (list :action-date schedule))
		     ,@(when user-id (list :app-user-id user-id))
		     ,@(when claim-id (list :claim-id claim-id))))
	 (query (s-sql:sql-compile `(:update diary-entry
					     :set ,@things
					     :where (:= diary-entry-id ,diary-entry-id)
					     :returning (:jsi.diary-entry diary-entry.*)))))

    (when (and deadline (not (string-equal deadline schedule)))
      (let* ((entry (find-diary-entry diary-entry-id))
	     (d (getjso "deadline" entry)))
	(when (not (string-equal deadline d))
	  (let ((string (format nil "INSERT INTO defer_diary_entry(diary_entry_id, defer_date) VALUES (~A, '~A')" diary-entry-id deadline)))
	    (postmodern:execute string)))))
    (ecm/database:query/return-jso query)))

(defun insert-diary-entry (&key
			     (processed nil processed-provided?)
			     claim-id
			     note
			     schedule
			     user-id)
  (let* ((things `(,@(when note (list :note note))
		     ,@(when processed-provided? (list :processed processed))
		     ,@(when schedule (list :action-date schedule))
		     ,@(when user-id (list :app-user-id user-id))
		     ,@(when claim-id (list :claim-id claim-id))))
	 (query (s-sql:sql-compile `(:insert-into diary-entry
						  :set ,@things
						  :returning (:jsi.diary-entry diary-entry.*)))))
    (ecm/database:query/return-jso query)))
		    

(defun diary-schedule-numbers (&optional (user (user)))
  (let ((schedule
	 (postmodern:query
	  (:select (:jsi.diary-schedule-numbers 'app-user)
		   :FROM 'app-user :where (:= 'app-user-id
					      (user-id user)))
	  :single)))
    (when schedule (ecm/json:read-json-from-string schedule))))

(defun outstanding-diary-entries-for-claim (claim-id &optional (user (user)))
  (let ((schedule
	 (postmodern:query
	  (:select (:jsi.diary-schedule-numbers 'app-user)
		   :FROM 'app-user :where (:= 'app-user-id
					      (user-id user)))
	  :single)))
    (when schedule (ecm/json:read-json-from-string schedule))))


