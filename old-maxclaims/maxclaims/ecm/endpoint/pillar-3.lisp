(defpackage :ecm/endpoint/pillar-3
  (:use :cl)
  (:import-from :ecm/user)
  (:import-from :ecm/entity/corpus)
  (:import-from :ecm/request-context)
  (:import-from :ecm/hunchentoot)
  (:import-from :ecm/ui/spreadsheet		
		#:<download-spreadsheet>)
  (:import-from :ecm/ui/report/pillar-3)
  (:import-from :ecm/report/pillar-3
		 #:syndicate-pillar-3-report-spreadsheet)
  (:import-from :ecm/endpoint
		#:define-endpoint)
  (:import-from :ecm/json #:getjso))

(in-package :ecm/endpoint/pillar-3)

(define-endpoint pillar-3 "/ecm/report/pillar-3")

(defun pillar-3/get ()
  (ecm/request-context:with-request-context ()
    (ecm/ui/report/pillar-3:pillar-3-page)))

(defun pillar-3/post ()
  (ecm/request-context:with-request-context ()
    (let* ((syndicate-id (ecm/hunchentoot:parameter-or-nil "syndicate-id"  :identity #'parse-integer))
	   (start-date (ecm/hunchentoot:parameter-or-nil "start-date"))
	   (end-date (ecm/hunchentoot:parameter-or-nil "end-date"))
	   (spreadsheet (ecm/hunchentoot:parameter-or-nil "spreadsheet-type")))
      (handler-case
	  (with-output-to-string (ecm/ml:*sexpml-output*)
	    (<download-spreadsheet>
	     (syndicate-pillar-3-report-spreadsheet
	      syndicate-id start-date end-date)
	     :type spreadsheet))
	(error (c)
	  (ecm/ui/report/pillar-3:pillar-3-page
	   :syndicate-id syndicate-id
	   :error c))))))



