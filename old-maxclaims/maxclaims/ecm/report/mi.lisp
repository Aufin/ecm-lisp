(defpackage :ecm/report/mi
  (:use :cl)
  (:import-from :ecm/spreadsheet
		#:cell-value)
  (:export #:syndicate-mi-report
	   #:contract-mi-report
	   #:contract-mi-report-spreadsheet
	   #:syndicate-mi-report-spreadsheet))
(in-package :ecm/report/mi)

(defun syndicate-mi-report (syndicate-id start-date end-date)
  "=> /json string/"
  ;(break "~A" syndicate-id)
  (postmodern:query
   (s-sql:sql-compile `(:select (to-json (mi-report ,start-date ,end-date ,syndicate-id))))
   :single))

(defun contract-mi-report (contract-id start-date end-date)
  "=> /json string/"
  (postmodern:query
   (s-sql:sql-compile `(:select (to-json (mi-report ,start-date ,end-date contract))
				:from contract :where (:= contract-id ,contract-id)))   
   :single))

(defparameter *mi-gnumeric-xml-pathname* 
  (merge-pathnames 
   "spreadsheet/Performance Management Information Indicators.xml"
   (asdf:system-source-directory 
    :maxclaims-ecm)))

(defun mi-report-empty-spreadsheet ()
  (ecm/spreadsheet:document :pathname *mi-gnumeric-xml-pathname*))

(defun syndicate-mi-report-spreadsheet (syndicate-id start-date end-date)
  (let* ((report-json
	  (syndicate-mi-report syndicate-id start-date end-date))
	 (report-jso
	  (if (equal report-json :null)
	      (ecm/json:jso)
	      (ecm/json:read-json-from-string
	       report-json)))
	 (ss (mi-report-empty-spreadsheet)))
    (macrolet ((cell (row text)
		 `(setf (cell-value ss ,row 1) (ecm/json:getjso
						,text report-jso))))
      (cell 1 "Volume of Open Claims")
      (cell 2 "Volume of Closed Claims")
      (cell 3 "Volume of Re-Opened Claims")
      (cell 4 "Volume of New Claims")
      (cell 5 "Value of open claims")
      (cell 6  "% of work referred to London?")
      (cell 7 "Volume of files held open for recovery/subrogation")
      
      (cell 9 "% Open claims referred to UW's outside of Market 5 day SLA")
      (cell 10 "Volume of Claims not acknowledged within Market 48 hours SLA")
      (cell 11 "Volume of Overdue Diary Items")
      (cell 12 "% of Open Claims Peer Reviewed")
      (cell 13 "Average days from 1st notification to 1st indemnity payment")
      (cell 14 "Average days from 1st notification to closed")
      (cell 15 "Average days to establish initial reserve from notice of claim")
      (cell 16 "Variance of final to initial reserve (as %)")
      (cell 17 "Volume of nil reserve claims older than 6 months (as %)")

      (cell 19 "Response time for acknowledging new claims")

      (cell 29 "How many full time Claims Examiners do you have?")

      #+(or)(ecm/spreadsheet:create-spreadsheet alists
						:format-dollarsign nil
						:calculate-totals nil)
      ss)))

(defun contract-mi-report-spreadsheet (contract-id start-date end-date)
  (let* ((report-json
	  (contract-mi-report contract-id start-date end-date))
	 (report-jso
	  (if (equal report-json :null)
	      (ecm/json:jso)
	      (ecm/json:read-json-from-string
	       report-json)))
	 (ss (mi-report-empty-spreadsheet)))
    (macrolet ((cell (row text)
		 `(setf (cell-value ss ,row 1) (ecm/json:getjso
						,text report-jso))))
      (cell 1 "Volume of Open Claims")
      (cell 2 "Volume of Closed Claims")
      (cell 3 "Volume of Re-Opened Claims")
      (cell 4 "Volume of New Claims")
      (cell 5 "Value of open claims")
      (cell 6  "% of work referred to London?")
      (cell 7 "Volume of files held open for recovery/subrogation")
      
      (cell 9 "% Open claims referred to UW's outside of Market 5 day SLA")
      (cell 10 "Volume of Claims not acknowledged within Market 48 hours SLA")
      (cell 11 "Volume of Overdue Diary Items")
      (cell 12 "% of Open Claims Peer Reviewed")
      (cell 13 "Average days from 1st notification to 1st indemnity payment")
      (cell 14 "Average days from 1st notification to closed")
      (cell 15 "Average days to establish initial reserve from notice of claim")
      (cell 16 "Variance of final to initial reserve (as %)")
      (cell 17 "Volume of nil reserve claims older than 6 months (as %)")

      (cell 19 "Response time for acknowledging new claims")

      (cell 29 "How many full time Claims Examiners do you have?")

      #+(or)(ecm/spreadsheet:create-spreadsheet alists
						:format-dollarsign nil
						:calculate-totals nil)
      ss)))




