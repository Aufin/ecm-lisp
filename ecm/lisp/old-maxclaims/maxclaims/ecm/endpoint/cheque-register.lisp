(defpackage :ecm/endpoint/cheque-register
  (:use :cl)
  (:import-from :ecm/user)
  (:import-from :ecm/entity/corpus)
  (:import-from :ecm/request-context)
  (:import-from :ecm/hunchentoot)
  (:import-from :ecm/report/cheque-register)
  (:import-from :ecm/ui/report/cheque-register)
  (:import-from :ecm/endpoint
		#:define-endpoint)
  (:import-from :ecm/json #:getjso))

(in-package :ecm/endpoint/cheque-register)

(define-endpoint agency "/ecm/cheque-register/agency")

(defun agency/get ()
  (ecm/request-context:with-request-context ()
    (ecm/ui/report/cheque-register:agency-cheque-register-page)))

(defun agency/post ()
  (ecm/request-context:with-request-context ()
    (let* ((agency-id (ecm/hunchentoot:parameter-or-nil "agency-id"  :identity #'parse-integer))
	   (start-time (ecm/hunchentoot:parameter-or-nil "start-time"))
	   (end-time (ecm/hunchentoot:parameter-or-nil "end-time"))
	   (risk (ecm/hunchentoot:parameter-or-nil "risk"))
	   (agency (ecm/entity/corpus:find-corpus agency-id)))      
      (handler-case
	  (let ((report (ecm/report/cheque-register:find-agency-cheque-register
			 agency-id :start-date start-time :end-date end-time :risk-type risk)))
	    (unless report (error "No Cheques for Period"))
	    (ecm/ui/report/cheque-register:agency-cheque-register-report-page
	     report agency  risk start-time end-time ))
									      
	(error (c)
	  (ecm/ui/report/cheque-register:agency-cheque-register-page
	   :agency-id agency-id :start-time start-time :end-time end-time :risk risk
	   :error c))))))

(define-endpoint contract "/ecm/cheque-register/contract")

(defun contract/get ()
  (ecm/request-context:with-request-context ()
    (ecm/ui/report/cheque-register:contract-cheque-register-page)))

(defun contract/post ()
  (ecm/request-context:with-request-context ()
    (let ((contract-id (ecm/hunchentoot:parameter-or-nil "contract-id"  :identity #'parse-integer))
	  (start-time (ecm/hunchentoot:parameter-or-nil "start-time"))
	  (end-time (ecm/hunchentoot:parameter-or-nil "end-time"))
	  (risk (ecm/hunchentoot:parameter-or-nil "risk")))
      (handler-case
	  (let ((report (ecm/report/cheque-register:find-contract-cheque-register
			 contract-id :start-date start-time :end-date end-time :risk-type risk)))
	    (unless report (error "No Cheques for Period"))

	    (ecm/ui/report/cheque-register:cheque-register-report-page report))
	(error (c)
	  (ecm/ui/report/cheque-register:contract-cheque-register-page
	   :contract-id contract-id :start-time start-time :end-time end-time :risk risk
	   :error c))))))

