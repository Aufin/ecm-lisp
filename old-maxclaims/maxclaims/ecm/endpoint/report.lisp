(defpackage :ecm/endpoint/report
  (:use :cl)
  (:import-from :ecm/user)
  (:import-from :ecm/entity/corpus)
  (:import-from :ecm/request-context)
  (:import-from :ecm/hunchentoot)
  (:import-from :ecm/ui/spreadsheet		
		#:<download-spreadsheet>)
  (:import-from :ecm/report/examiner-claim)
  (:import-from :ecm/report/peer-review
                #:peer-review-spreadsheet)
  (:import-from :ecm/ui/report/peer-review
                #:peer-review-page)
  (:import-from :ecm/endpoint
		#:define-endpoint)
  (:import-from :ecm/ui/report)
  (:import-from :ecm/ui/report/bordereau)
  (:import-from :ecm/report/bordereau)
  (:import-from :ecm/json #:getjso))

(in-package :ecm/endpoint/report)

(define-endpoint report "/ecm/report/?$")

(defun report/get ()
  (ecm/request-context:with-request-context ()
    (ecm/ui/report::report-page)))

(define-endpoint peer-review "/ecm/report/peer-review")

(defun peer-review/get ()
  (peer-review-page))

(defun peer-review/post ()

  (ecm/request-context:with-request-context ()
  (let ((ss (peer-review-spreadsheet))
        (spreadsheet-type (ecm/hunchentoot:parameter-or-nil "spreadsheet-type")))
    (<download-spreadsheet> ss :filename "Peer Review Outstanding"
                               :type spreadsheet-type))))

(define-endpoint examiner-claims "/ecm/report/examiner-claims/(.*)/?$")

(defun examiner-claims/get (examiner-id)
  (ecm/request-context:with-request-context ()
    (let* ((report (ecm/report/examiner-claim:examiner-claims
		    (parse-integer examiner-id)))
	   (ss (ecm/report/examiner-claim:examiner-claims-spreadsheet
		report)))
    (<download-spreadsheet> ss :type "Gnumeric_Excel:xlsx"
			    :filename "My Claims"))))

(define-endpoint bordereau "/ecm/report/bordereau/(.*)?/?$")

(defun bordereau/get (type)
  (ecm/request-context:with-request-context ()
    (let ((contract-id (ecm/hunchentoot:parameter-or-nil
			                  "contract-id"
			                  :identity #'parse-integer)))
      (if contract-id
	        (ecm/ui/report::bordereau-page
           type :contract-id contract-id)
	        (ecm/ui/report::bordereau-page type)))))

(define-condition handled-error () ())

(defun bordereau/post (type)
  (ecm/request-context:with-request-context ()
    (let ((contract-id (ecm/hunchentoot:parameter-or-nil
			                  "contract-id"
			                  :identity #'parse-integer))
          (syndicate-id (ecm/hunchentoot:parameter-or-nil
			                   "syndicate-id"
			                   :identity #'parse-integer))
          #+(or)(only-open-claims? (ecm/hunchentoot:parameter-or-nil
                              "open-claims-only"))
	        (start-date (ecm/hunchentoot:parameter-or-nil "start-date"))
	        (end-date (ecm/hunchentoot:parameter-or-nil "end-date"))
	        (risk-type (ecm/hunchentoot:parameter-or-nil "risk-type"))
	        (new-window (ecm/hunchentoot:parameter-or-nil "new-window"))
	        (spreadsheet (ecm/hunchentoot:parameter-or-nil "spreadsheet"))
	        (spreadsheet-type(ecm/hunchentoot:parameter-or-nil "spreadsheet-type")))
      ;; (let ((ecm/report/bordereau:*open-claims-only*
      ;;         only-open-claims?))
        (handler-case
	          (if (not (and (or syndicate-id contract-id #+(or) only-open-claims?) start-date end-date))
	              (error 'handled-error)
	              (let ((report
                        (cond
                          ((equalp type "ascot")
                           (ecm/report/bordereau::ascot-bordereau
                            contract-id syndicate-id start-date end-date risk-type))
                          ((equalp type "hiscox")
                           (ecm/report/bordereau::hiscox-bordereau
                            contract-id syndicate-id start-date end-date risk-type))
                          ((equalp type "enstar")
                           (ecm/report/bordereau::enstar-bordereau
                            contract-id syndicate-id start-date end-date risk-type))
                          ((equalp type "lloyds-v5")
                           (ecm/report/bordereau::lloyds-v5-bordereau
                            contract-id syndicate-id start-date end-date risk-type))
                          ((equalp type "zurich")
                           (ecm/report/bordereau::zurich-bordereau
                            contract-id syndicate-id start-date end-date risk-type))
                          (:else
		                   (funcall
                            (intern
			                 (string-upcase (format nil "~A-bordereau" type))
			                 :ecm/report/bordereau)
			                contract-id
			                start-date
			                end-date risk-type)))))
		              (cond (new-window
		                     (ecm/ui/report/bordereau::bordereau-report-page
			                    contract-id start-date end-date risk-type report))
		                    (spreadsheet
		                     (multiple-value-bind (ss filename)
                                 (let ((args `(,contract-id
                                               ,@(when (member
                                                        type '("lloyds-v5" "zurich" "enstar" "ascot" "hiscox")
                                                        :test #'equalp)
                                                   (list syndicate-id))
                                               ,start-date ,end-date ,risk-type
                                               :bordereau ,report)))
                                   (apply
                                    (intern
				                     (string-upcase (format nil "~A-bordereau-spreadsheet" type))
				                     :ecm/report/bordereau)
                                    args))
		                       (<download-spreadsheet> ss :type spreadsheet-type
					                                      :filename filename))))))
	        #+(or)(error (c)
	                (error c)#+(or)(ecm/ui/report::bordereau-page
	                                type
	                                :error c
	                                :contract-id contract-id))
	        (handled-error ()
	          (ecm/ui/report::bordereau-page
	           type
	           :contract-id contract-id))))))

;;)
