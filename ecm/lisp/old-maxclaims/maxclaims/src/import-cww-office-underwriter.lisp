(in-package #:maxclaims)

;;; Import CWW June 2011 Policy Office and Underwriter data

(defparameter *cww-csv-import-file*
  #P"/home/clinton/work/maxclaims/Maxwell\ Claims\ -\ Update.csv")

(defun unique-office/underwriter (&optional (path *cww-csv-import-file*))
  (loop for row in (cdr (read-csv-file path))
     collect (third row) into offices
     collect (fourth row) into underwriters
     finally (return (values (remove-duplicates offices :test #'string=)
			     (remove-duplicates underwriters :test #'string=)))))

(defun find-cww-underwriter-record (name)
  ;; Format in Spreadsheet is LAST, FIRST
  ;; Munge that into a usable format and try to find the PERSON record
  (ppcre:register-groups-bind (last-name first-name)
      ("(\\w+), (\\w+)" name)
    (flet ((%ize (s) (format nil "%~A%" s)))
      (select-objects 'person :where
		      `(:or (:and (:% 'first-name ,first-name)
				  (:% 'last-name ,last-name))
			    (:and (:ilike 'first-name ,(%ize first-name))
				  (:ilike 'last-name ,(%ize last-name))))))))

(defun find-cww-office-record (name)
  ;; Format in Spreadsheet is [LOCATION - ]NAME
  ;; Munge that into a usable format and try to find the PERSON record
  (ppcre:register-groups-bind (location agency)
      ("(?:((?:\\w|\\s)+) - )*((?:\\w|\\s)+)" name)
    #+nil(break "~A / ~A" location agency)
    (query-objects 'person
		   (lambda (table fields)
		     `(:select ,@fields
			       :from ,table :natural :left-join 'province-state
			       :where (:and (:or (:is-null 'first-name)
						 (:= 'first-name "")) ; yep
					    (:% 'company-name ,agency)
					    ,@(when location
						    `((:or (:% 'city ,location)
							    (:% 'long-name ,location))))))))))

(defun seed-import-tables ()
  (multiple-value-bind (offices underwriters)
      (unique-office/underwriter)
    (with-transaction ()
      (dolist (o offices)
	(query (:insert-into 'cww-import-office :set 'name o)))
      (dolist (u underwriters)
	(query (:insert-into 'cww-import-underwriter :set 'name u))))))

;;; Most of the office records do not exist and need to be created
;;;  - Need to display at least part of the location information for
;;;    PERSON to differentiate between regional offices of the same
;;;    company

;;; Most of the Underwriters *do* exist but some appear to not, and a
;;; few have duplicates.

;;; A web interface to display/find/create the required reocrds is
;;; probably best... needs to store the string -> PERSON map somewhere
;;; (either temp db table or csv?)

(defclass cww-import-underwriter ()
  ((name :primary-key t)
   (person :column person-id :references person
	   :input (:type select-db-object :db-type person)
	   :activate (editable)
	   :attributes (first-name last-name company-name city province/state))
   person-id)
  (:metaclass described-db-access-class)
  (:attributes name person matches))

(defclass cww-import-office ()
  ((name :primary-key t)
   (person :column person-id :references person
	   :input (:type select-db-object :db-type person)
	   :activate (editable)
	   :attributes (company-name city province/state))
   person-id)
  (:metaclass described-db-access-class)
  (:attributes name person matches))

(define-description cww-import-underwriter (description-for-cww-import-underwriter)
  ((matches :function (compose 'find-cww-underwriter-record 'cww-import-underwriter.name)
	    :label "Matches"
	    :attributes ((list :item (:activate (link-to-viewer)))))))

(define-description cww-import-office (description-for-cww-import-office)
  ((matches :function (compose 'find-cww-office-record 'cww-import-office.name)
	    :label "Matches"
	    :attributes ((list :item (:activate (link-to-viewer)))))))

(defcomponent cww-office-underwriter-import ()
  ((offices :initform (select-objects 'cww-import-office))
   (underwriters :initform (select-objects 'cww-import-underwriter))))

(defmethod cww-update-instances ((self cww-office-underwriter-import))
  ;; A bit brute force but... one time use form &c
  (with-transaction ()
    (dolist* (o (slot-value self 'offices))
      (update-object o))
    (dolist* (u (slot-value self 'underwriters))
      (update-object u))))


(defmethod render ((self cww-office-underwriter-import))
  (<ucw:form :method "POST" :action (refresh-component self)
     (<ucw:submit :action (cww-update-instances self)
		  "SAVE")
     (<:h1 "CWW Office/Underwriter Import")
     (<:p "You must select SAVE to commit changes to the database.")
     (<:p "Select or Create Office/Underwriter records for the CWW
      import. Possible existing records are displayed for reference." )
     
     (<:h2 "Offices")
     (display self (slot-value self 'offices)
	      :attributes '(list))
     (<:h2 "Underwriters")
     (display self (slot-value self 'underwriters)
	      :attributes '(list))))