(defpackage :ecm/endpoint/api
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
  (:import-from :ecm/entity/bordereau
                #:bordereau->json
                #:get-bordereau-instance)
  (:import-from :ecm/ui/report/static-claims)
  (:import-from :ecm/entity/api/static
		#:run-static-claims-report
		#:static-claims-report-pathname)
  (:import-from :ecm/json #:getjso))

(in-package :ecm/endpoint/api)

(define-endpoint static-claims "/ecm/report/api/static-claims")

(defun static-claims/get ()
  (ecm/request-context:with-request-context ()
    (ecm/ui/report/static-claims::static-claims-page)))

(defun static-claims/post ()
  (ecm/request-context:with-request-context ()
    (let* ((interval (ecm/hunchentoot:parameter-or-nil "interval"))
	   (scr (run-static-claims-report interval))
	   (pathname (static-claims-report-pathname scr))
	   (content-disposition "attachment")
	   (content-type "text/comma-separated-values")
	   (filename "Static-Claims"))
      (setf (ecm/hunchentoot:header-out
	     "Content-Disposition")
	    (format nil "~A; filename=~W"
		    content-disposition filename))
      (ecm/hunchentoot:handle-static-file
       pathname content-type))))

(define-endpoint bordereau-api-meta "/ecm/report/api/bordereau/meta/?$")

(defun bordereau-api-meta/get ()
  (ecm/request-context:with-request-context ()
    (handler-case
	(let ((ecm/request-context:*json-mime-type* "application/json"))
	  (ecm/request-context:send-json-response
	   (ecm/entity/bordereau:bordereau-meta-as-json-string) :raw t))
      (error (c)
        (ecm/request-context:send-json-response
         (ecm/json:jso "error" (format nil "~A" c)))))))

(define-endpoint bordereau-api-run "/ecm/report/api/bordereau/run/(.*)$")
(defun bordereau-api-run/get (name)
  (ecm/request-context:with-request-context ()
    (handler-case
        (let ((bdx (get-bordereau-instance name)))
          (ecm/request-context:send-json-response
           (bordereau->json bdx)))
      (error (c)
        (ecm/request-context:send-json-response
         (ecm/json:jso "error" (format nil "~A" c)
                       "name" name))))))

(define-endpoint bordereau-api "/ecm/report/api/bordereau/?$")

(defun bordereau-api/get (&aux (type "lloyds-v5"))
  (ecm/request-context:with-request-context ()
    (let ((contract-id (ecm/hunchentoot:parameter-or-nil
			                  "contract-id"
			                  :identity #'parse-integer)))
      (if contract-id
	        (ecm/ui/report/bordereau-api::bordereau-api-page
           :type type :contract-id contract-id)
	        (ecm/ui/report/bordereau-api::bordereau-api-page :type type)))))

(define-condition handled-error () ())

(defun bordereau-api/post ()
  (ecm/request-context:with-request-context ()
    (let ((type (ecm/hunchentoot:parameter-or-nil
			           "bordereau-type"))
          (for (ecm/hunchentoot:parameter-or-nil
			           "bordereau-for"))
          (contract-id (ecm/hunchentoot:parameter-or-nil
			                  "contract-id"
			                  :identity #'parse-integer))
          (syndicate-id (ecm/hunchentoot:parameter-or-nil
			                   "syndicate-id"
			                   :identity #'parse-integer))
          (agency-id (ecm/hunchentoot:parameter-or-nil
			                   "agency-id"
			                   :identity #'parse-integer))
	  (start-date (ecm/hunchentoot:parameter-or-nil "start-date"))
	  (end-date (ecm/hunchentoot:parameter-or-nil "end-date"))
	  (risk-type (ecm/hunchentoot:parameter-or-nil "risk-type")))
          (handler-case
              (let* ((bdx-type (intern type :ecm/entity/bordereau))
                     (bdx-for (intern for :ecm/entity/bordereau))
                     (bdx-id (case (intern for :ecm/endpoint/api)
                               (contract contract-id)
                               (agency agency-id)
                               (syndicate syndicate-id)))
                      (bdx (ecm/entity/bordereau:make-bordereau-report
                            :type bdx-type :for bdx-for :id bdx-id
                            :start-date start-date :end-date end-date :risk-type risk-type
                            )))

                  (ecm/request-context:send-json-response
                   (bordereau->json bdx)))
	        (error (condition)
            (ecm/request-context:send-json-response
             (ecm/json:jso
              "error"
              (format nil "CONDITION: ~A ~&" condition))))))))

(define-endpoint bordereau-download
  "/ecm/report/api/bordereau/download/(.*)$")

(defun bordereau-download/get (name)
  (ecm/request-context:with-request-context ()
    (let* ((bdx (get-bordereau-instance name))
	         (pathname (ecm/entity/api/bordereau::bordereau-pathname bdx))
	         (content-disposition "attachment")
	         (content-type "text/comma-separated-values")
	         (filename (ecm/hunchentoot:parameter-or-nil
			                   "bdx-filename")))
      (setf (ecm/hunchentoot:header-out
	     "Content-Disposition")
	    (format nil "~A; filename=~W"
		          content-disposition filename))
      (ecm/hunchentoot:handle-static-file
       pathname content-type))))
