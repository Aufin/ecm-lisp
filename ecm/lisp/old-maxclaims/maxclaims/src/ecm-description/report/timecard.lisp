(defpackage :maxclaims/ecm-description/report/timecard 
  (:use) 
  (:import-from :maxclaims/report/timecard)
  (:export #:make-timecard-report))

(in-package :maxclaims/ecm-description)

(defclass timecard-report ()
  ((report :initarg report
	   :reader cheque-register-report)))

(defclass timecard-report-line ()
  ((line :initarg line)))

(defun make-timecard-report-line (line)
  (make-instance 'timecard-report-line 'line line))

(defun maxclaims/ecm-description/report/timecard:make-timecard-report 
    (report-list)
  (make-instance
   'timecard-report
   'report (mapcar #' make-timecard-report-line report-list)))

(define-descriptions timecard-report 
  (:default (report :as-table t 
		    :label "Timecards"
		    :layers :default))
  (:view report)
  (:view-tab)
  (:heading (:value :activate ((h2)) 
		    :value "Timecards October Report")))

(defmethod insert-object ((o timecard-report))
  o)

(defmethod object-attribute-value ((l timecard-report-line) attribute-name
				   &key &allow-other-keys)
  (or (cdr (assoc (substitute #\Space #\- (string-capitalize (string attribute-name)))
	      (slot-value l 'line):test #'string-equal))
      (princ-to-string (slot-value l 'line))))

(defmethod object-attribute-value ((l timecard-report-line) (name (eql 'incurred))
				   &key &allow-other-keys)
  (format nil "$~$" (call-next-method)))

(defmethod object-attribute-value ((l timecard-report-line) (name (eql 'authority))
				   &key &allow-other-keys)
  (format nil "$~$" (call-next-method)))

(define-descriptions timecard-report-line 
  (:default 
      (|claim number| :label t)
      (examiner :label t)
    (time :label t)
    (disbursements :label t)
    (mileage :label t)
    (notes :label t))
  (:inline
   (identity :label t)
   (examiner :label t)

   ))
    




	

  
