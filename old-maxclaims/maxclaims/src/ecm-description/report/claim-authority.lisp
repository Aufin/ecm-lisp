(defpackage :maxclaims/ecm-description/report/claim-authority 
  (:use) 
  (:import-from :maxclaims/report/claim-authority)
  (:export #:make-claim-authority-report))

(in-package :maxclaims/ecm-description)

(defclass claim-authority-report ()
  ((report :initarg report
	   :reader cheque-register-report)))

(defclass claim-authority-report-line ()
  ((line :initarg line)))

(defun make-claim-authority-report-line (line)
  (make-instance 'claim-authority-report-line 'line line))

(defun maxclaims/ecm-description/report/claim-authority:make-claim-authority-report 
    (report-list)
  (make-instance
   'claim-authority-report
   'report (mapcar #' make-claim-authority-report-line report-list)))

(define-descriptions claim-authority-report 
  (:default (report :as-table t 
		    :label "Claims over Authority"
		    :layers :default))
  (:view report)
  (:view-tab)
  (:heading (:value :activate ((h2)) 
		    :value "Claim Authority Report")))

(defmethod insert-object ((o claim-authority-report))
  o)

(defmethod object-attribute-value ((l claim-authority-report-line) attribute-name
				   &key &allow-other-keys)
  (or (cdr (assoc (substitute #\Space #\- (string-capitalize (string attribute-name)))
	      (slot-value l 'line):test #'string-equal))
      (princ-to-string (slot-value l 'line))))

(defmethod object-attribute-value ((l claim-authority-report-line) (name (eql 'incurred))
				   &key &allow-other-keys)
  (format nil "$~$" (call-next-method)))

(defmethod object-attribute-value ((l claim-authority-report-line) (name (eql 'authority))
				   &key &allow-other-keys)
  (format nil "$~$" (call-next-method)))

(define-descriptions claim-authority-report-line 
  (:default 
      (|claim number| :label t)
      (examiner :label t)
    (incurred :label t)
    (authority :label t))
  (:inline
   (identity :label t)
   (examiner :label t)

   ))
    




	

  
