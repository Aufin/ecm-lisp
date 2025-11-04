(defpackage :maxclaims/report/timecard
  (:use :cl)
  (:import-from :rofl 
		#:select)
  (:import-from :maxclaims
		  #:with-adb
		  #:risk-type.type-name)
  (:export 
	   #:select-timecards))

(in-package :maxclaims/report/timecard)

;; (let ((*package* (find-package :maxclaims/report/timecard)))
;; 	      (print (maxclaims::with-adb (list
;; 		      (postmodern:query "select column_name from INFORMATION_SCHEMA.COLUMNS where table_name = 'timecard';" )
;; 		      (map 'list 'c2mop:slot-definition-name  (c2mop:class-slots (find-class 'maxclaims/ecm-description::timecard)))))))
	     
;; ((("disbursements") ("mileage_km") ("notes") ("minutes") ("date")
;;   ("app_user_id") ("claim_id") ("timecard_id"))
;;  (RELATIONAL-OBJECTS-FOR-LISP::%PERSISTENT/MODIFICATIONS
;;   RELATIONAL-OBJECTS-FOR-LISP::%FOREIGN-MODIFICATIONS
;;   MAXCLAIMS::TIMECARD-ID MAXCLAIMS::CLAIM-ID MAXCLAIMS::CLAIM
;;   MAXCLAIMS::APP-USER MAXCLAIMS::DATE MAXCLAIMS::MINUTES MAXCLAIMS::NOTES
;;   MAXCLAIMS::MILEAGE-KM MAXCLAIMS::DISBURSEMENTS MAXCLAIMS::APP-USER-ID)) 

(defparameter *timecard-headings*
  '(("Claim Number" c.claim-id)
    ("Examiner" (:raw "person_name(find_person(c.adjuster_id))"))
    ("Time" (:type (:sum ti.minutes) string))
    ("Disbursements" (:type (:sum ti.disbursements) string))
    ("Mileage" (:type (:sum ti.mileage_km) string))
    ("Notes" (:array-agg ti.notes))))

(defparameter *timecard-query* 
  '(:from (:as timecard ti)
    :left-join (:as claim c)
    :on (:= c.claim-id ti.claim-id)
    :where (:and (:>= ti.date "2013-10-01")
	    (< ti.date "2013-11-01"))))

(defstruct (timecard-heading (:type list))
  name s-sql)

(defun heading-as (heading)
  `(:as ,(timecard-heading-s-sql heading)
	(:raw ,(concatenate 'string "\""
		      (timecard-heading-name heading)
		      "\""))))
(defun timecard-query ()
  `(:select 
    '* 
    :from (:as 
	   (:order-by 
	    (:select   
	     ,@(mapcar #'heading-as *timecard-headings*)
	     :distinct
	     ,@*timecard-query*	     
	     :group-by c.claim-id)
	    c.claim-id)
	   tc)))

(defun select-timecards ()
  (postmodern:query
   (s-sql:sql-compile (timecard-query))
   :str-alists))
  

