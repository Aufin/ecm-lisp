(defpackage :maxclaims/ecm-description/interim
  (:use))

(in-package :maxclaims/ecm-description)

(defclass interim-report ()
  ((report :initarg :report
	   :reader bordereau-report)
   (minutes
    :initarg :minutes
    :initform 15
    :accessor interim-report-minutes)
   
   (spreadsheet-type 
    :initarg :spreadsheet-type
    :initform nil
    :accessor interim-report-spreadsheet-type)))

(defmethod insert-object ((o  interim-report))
  o)


(define-descriptions interim-report
  (:default (report :as-table t 		    
		    :label t
		    :layers :default)
      (minutes :label "Minimum Billable Hours"))
  (spreadsheet-type
   :label t
   :type ssconvert-type
   :select-objects (ssconvert-export-types))
  (:heading (:value :label "report"
		    :value "Interim"
		    :activate (h3)))
  (:view-tab)
  (:view)
  
  (:create  
   minutes
   (spreadsheet-type
    :label t
    :type ssconvert-type
    :select-objects (ssconvert-export-types)))
  (:create-heading (:value :label "create report"
		    :value "Interim"
		    :activate (h3))))
