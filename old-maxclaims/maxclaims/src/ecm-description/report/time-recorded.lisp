(defpackage :maxclaims/ecm-description/time-recorded
  (:use))

(in-package :maxclaims/ecm-description)

(defclass time-recorded-report ()
  ((report :initarg :report
	   :reader bordereau-report)
   (start-date 
    :initarg :start-date
    :initform (multiple-value-bind (year month) 
		  (simple-date:decode-timestamp 
		   (simple-date:universal-time-to-timestamp 
		    (get-universal-time)))
		(simple-date:encode-date year month 01)))
   (end-date 
    :initarg :end-date
    :initform (multiple-value-bind (year month day) 
		  (simple-date:decode-timestamp 
		   (simple-date:universal-time-to-timestamp 
		    (get-universal-time)))
		(simple-date:encode-date year month day)))
   (spreadsheet-type 
    :initarg :spreadsheet-type
    :initform nil
    :accessor time-recorded-report-spreadsheet-type)))

(defmethod insert-object ((o  time-recorded-report))
  o)


(define-descriptions time-recorded-report
  (:default (report :as-table t 		    
		    :label t
		    :layers :default)
      (contract :label "Contract")
    (spreadsheet-type
     :label t
     :type ssconvert-type
     :select-objects (ssconvert-export-types)))
  (:heading (:value :label "report"
		    :value "Time Recorded"
		    :activate (h3)))
  (:view-tab)
  (:view  
   (start-date 
    :label t
    :active t
    :type simple-date:date
    :required t)
   (end-date 
    :label t
    :active t
    :type simple-date:date
    :required t))
  
  (:create  
   (start-date 
    :label t
    :active t
    :type simple-date:date
    :required t)
   (end-date 
    :label t
    :active t
    :type simple-date:date
    :required t) 
   (spreadsheet-type
    :label t
    :type ssconvert-type
    :select-objects (ssconvert-export-types)))
  (:create-heading (:value :label "create report"
		    :value "Time Recorded"
		    :activate (h3))))
  
