(defpackage :ecm/report/pillar-3
  (:use :cl)
  (:import-from :ecm/spreadsheet
		#:cell-value)
  (:export #:syndicate-pillar-3-report
	   #:syndicate-pillar-3-report-spreadsheet))
(in-package :ecm/report/pillar-3)

(defun syndicate-pillar-3-report (syndicate-id start-date end-date)
  "=> /alist/"
  ;(break "~A" syndicate-id)
  (postmodern:query
   (s-sql:sql-compile
    `(:select * :from (pillar-3 ,syndicate-id ,start-date ,end-date)))
   :str-alists))

(defparameter *pillar-3-gnumeric-xml-pathname* 
  (merge-pathnames 
   "spreadsheet/Pillar 3 Report.xml"
   (asdf:system-source-directory 
    :maxclaims-ecm)))

(defun pillar-3-report-empty-spreadsheet ()
  (ecm/spreadsheet:document :pathname *pillar-3-gnumeric-xml-pathname*))

(defun syndicate-pillar-3-report-spreadsheet (syndicate-id start-date end-date)
  (ecm/spreadsheet:create-spreadsheet
   (syndicate-pillar-3-report syndicate-id start-date end-date)
;   :document (pillar-3-report-empty-spreadsheet)
 ;  :create-header nil
  ; :document-sheet-name "Sheet1"
   ;:use-document-columns t
   :start-row 0))




