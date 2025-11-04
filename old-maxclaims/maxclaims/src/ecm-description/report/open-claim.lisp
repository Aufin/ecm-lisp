(defpackage :maxclaims/ecm-description/open-claim
  (:use))

(in-package :maxclaims/ecm-description)

(defclass open-claim-report ()
  ((report :initarg :report
	   :reader bordereau-report)
   (spreadsheet-type 
    :initarg :spreadsheet-type
    :initform nil
    :accessor open-claim-report-spreadsheet-type)))

(defmethod insert-object ((o  open-claim-report))
  o)


(define-descriptions open-claim-report
  (:default (report :as-table t 		    
		    :label t
		    :layers :default)
    (spreadsheet-type
     :label t
     :type ssconvert-type
     :select-objects (ssconvert-export-types)))
  (:heading (:value :label "report"
		    :value "Claims Open"
		    :activate (h3)))
  (:view-tab)
  (:view)
  
  (:create   
   (spreadsheet-type
    :label t
    :type ssconvert-type
    :select-objects (ssconvert-export-types)))
  (:create-heading (:value :label "create report"
		    :value "Claims Open"
		    :activate (h3))))

  
