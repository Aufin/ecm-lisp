(defpackage :ecm/report/peer-review
  (:use :cl)
  (:import-from #:ecm/spreadsheet #:create-spreadsheet)
  (:export #:peer-review-spreadsheet))
(in-package :ecm/report/peer-review)

(defun peer-review-report ()
  (postmodern:query
   "SELECT * FROM peer_review
     WHERE date IS NULL AND status = 'Open' ORDER BY syndicate, claim_id"
   :str-alists))

(defun peer-review-spreadsheet ()
  (create-spreadsheet
   (peer-review-report) :format-dollarsign nil :calculate-totals nil))
