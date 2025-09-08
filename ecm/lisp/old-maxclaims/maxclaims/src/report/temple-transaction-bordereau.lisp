(defpackage :maxclaims/report/temple-transaction-bordereau
  (:use :cl)
  (:import-from :maxclaims
                #:with-adb))
(in-package :maxclaims/report/temple-transaction-bordereau)
                                   
(defparameter *temple-transaction-bordereau* 
  '((:as "Claim Number" "TPA Claim Number") 
    "Policy Number" 
    (:as "Unknown" "Program")
    (:as "Unknown" "Temple Claim #")
    "Insured" 
    "Policy Effective" 
    "Policy Expiry" 
    (:as "Unknown"  "Loss Address")
    (:as "Unknown"  "Loss City")
    (:as "Unknown"  "Loss Province")
    (:as "Unknown"  "Loss Postal Code")
    "Status" 
    "Date of Loss" 
    "Date Reported" 
    "Close Date" 
    "Reopen Date" 
    "Transaction Authorization Date" 
    "Loss Description" 
    "Cause of Loss" "Kind of Loss Description/Code"
    (:as "Unknown"  "Even Category")
    (:as "Unknown"  "Basis o Settlement")
    (:as "Unknown" "LOB")
    (:as "Unknown" "Coverage")
    "Transaction Type"
    "Heading"
    "Entity Type"
    "Transaction Amount"
    (:as "Unknown" "Subscription %")
    "Pay to (payee)"
    "Total Current Loss Reserve"
    "Total Current Expense Reserve"
    (:as "Paid Loss" "Temple Share Paid Loss")
    (:as "Paid Expense" "Temple Share Paid Expense")
    (:as "Recovery" "Temple Share Recovery")
    (:as "Incurred" "Temple Share Incurred")))
  
(defun s-sql/temple-transaction-bordereau-headings ()
  (loop for h in *temple-transaction-bordereau*
       :collect (if (listp h) 
                   `(:as (:raw ,(write-to-string (second h)))
                         (:raw ,(write-to-string (third h))))
                   `(:raw ,(write-to-string h)))))

(defun s-sql/select-temple-transaction-bordereau ()
  `(:SELECT 
    ,@(s-sql/temple-transaction-bordereau-headings)
     :from transaction-bordereau))

(defun s-sql/create-view-temple-transaction-bordereau ()
  `(:create-view 
    temple-transaction-bordereau
    ,(s-sql/select-temple-transaction-bordereau)))

(defun migration-temple-transaction-bordereau-form (&optional (number 73))
  `(define-migration ,number
       ,(s-sql:sql-compile (s-sql/create-view-temple-transaction-bordereau))
     ,(s-sql:sql-compile 
       `(:drop-view :if-exists temple-transaction-bordereau))))
