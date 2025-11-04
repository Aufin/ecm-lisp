(defpackage :ecm/entity/bordereau
  (:use :cl)
  (:import-from :ecm/entity/api/bordereau
                #:bordereau
                #:bordereau-json
                #:get-bordereau-instance)
  (:export #:make-bordereau-report
           #:list-bordereau-types
           #:list-bordereau-fors
           #:bordereau->json
           #:get-bordereau-instance
	   #:bordereau-meta-as-json-string))

;; https://superuser.com/questions/1135850/how-do-i-run-a-libreoffice-macro-from-the-command-line-without-the-gui
(in-package :ecm/entity/bordereau)

(import 'maxclaims/report/bordereau::lloyds-v5-bordereau-fields)
;; A bordereau has:
;; - a species AKA type (lloydsv5, arch, white-oak etc)
;; - a genus AKA for (contract/syndicate/agency)
;; - a date range :: Start Date and End Date
;; - Optionally, a risk type via
;;     (dolist (r (ecm/entity/risk:risk-types)) ...)

(defun bordereau-meta-as-json-string ()
  (ecm/database:query "SELECT api.bordereau_meta()" :single))

(defparameter *bordereau-type-alist*
  '((lloyds-v5
     :fields maxclaims/report/bordereau::lloyds-v5-bordereau-fields)
    #|(Accelerant 
     :fields maxclaims/report/bordereau::accellerant-bordereau-fields) |#
    (casualty-liability
     :fields maxclaims/report/bordereau::default-bordereau-fields)
    (ascot-lloyds-v5-denial
     :fields maxclaims/report/bordereau:ascot-lloyds-v5-bordereau-fields)
    (hiscox-positional
     :fields maxclaims/report/bordereau:hiscox-bordereau-fields
     :prefix-currency nil)
    (agency
     :fields maxclaims/report/bordereau::agency-bordereau-fields)
    (hub-dale
     :fields maxclaims/report/bordereau:HUB-Dale-bordereau-fields)
    (lloyds-claims-report
     :fields maxclaims/report/bordereau::llyods-bordereau-fields)
    (commonwell-lloyds
     :fields maxclaims/report/bordereau::llyods-bordereau-fields)
    (arch
     :fields maxclaims/report/bordereau:arch-bordereau-fields)
    (inter-hannover
     :fields maxclaims/report/bordereau:inter-hannover-bordereau-fields)
    (white-oak
     :fields maxclaims/report/bordereau:white-oak-bordereau-fields)
    (property-claims
     :fields maxclaims/report/bordereau:claim-bordereau-fields)
    (zurich
     :fields maxclaims/report/bordereau::zurich-bordereau-fields)
    (enstar
     :fields maxclaims/report/bordereau:lloyds-v5-bordereau-fields)))

(defun bordereau-database-json (bdx)
  (ecm/json:read-json-from-string
   (postmodern:query
    (if (string-equal
         (princ-to-string (bordereau-report-for bdx)) "CONTRACT")
        "SELECT contract_to_json($1::int)"
        "SELECT person_to_json($1::int)")
    (bordereau-report-id bdx)
    :single)))

(defun bordereau->json (bdx)
  (ecm/json:jso
   "name" (ecm/entity/api/bordereau::bordereau-id bdx)
   "status" (let ((status(ecm/entity/api/bordereau::bordereau-status bdx)))
              (if status
                  (format nil "~:(~a~)" (princ-to-string status))
               "Created"))
   "pathname" (princ-to-string (ecm/entity/api/bordereau::bordereau-pathname bdx))
   "run_start_time"
   (or (ecm/entity/api/bordereau::bordereau-run-start-time bdx)
       :false)
   "run_end_time"
   (or (ecm/entity/api/bordereau::bordereau-run-end-time bdx)
       :false)

   "for" (format nil "~:(~a~)" (substitute #\Space #\-
                                           (symbol-name  (bordereau-report-for bdx))))
   "type" (format nil "~:(~a~)" (substitute #\Space #\-
                                            (symbol-name  (bordereau-report-type bdx))))
   "start_date" (ecm/entity/api/bordereau::bordereau-start-date bdx)
   "end_date" (ecm/entity/api/bordereau::bordereau-end-date bdx)
   "meta" (format nil "ERROR: ~A" (ecm/entity/api/bordereau::bordereau-meta bdx))
   "id" (bordereau-report-id bdx)
   "risk-type" (or (ecm/entity/api/bordereau::bordereau-risk-type bdx)
                   :null)
   "json" (or (ecm/entity/api/bordereau::bordereau-json bdx)
              (setf  (ecm/entity/api/bordereau::bordereau-json bdx)
                     (maxclaims::with-adb (bordereau-database-json bdx))))))



(defun list-bordereau-types () (mapcar #'car *bordereau-type-alist*))

(defvar *bordereau-for-list*
  '(contract syndicate agency))

(defun list-bordereau-fors () *bordereau-for-list*)

(defun make-bordereau-where-for (for value)
  (let* ((for-id (intern (format nil "CONTRACT.~A-ID" for)))
         (single-where (lambda (v)
                         `(:= ,for-id ,v))))
    (if (listp value)
        `(:or ,@(mapcar single-where value))
        (funcall single-where value))))

(defstruct (bordereau-report (:include bordereau)
                             (:constructor make-%bordereau-report))
  type for id)

(defun make-bordereau-report
    (&key (type 'lloyds-v5)
       (for 'contract)
       (id 1535)
       (start-date "2023-01-01")
       (end-date "2023-02-01")
       (risk-type nil))
  (let* ((props (cdr (assoc type *bordereau-type-alist*)))
         (fields-prop (getf props :fields))
         (fields (lambda (&rest _)
                   (declare (ignore _))
                   (funcall fields-prop)))
	 (prefix-currency (getf props :prefix-currency t))
         (where-for (make-bordereau-where-for for id))
         (where (constantly where-for)))
    (ecm/entity/api/bordereau:make-bordereau-instance
     '%bordereau-report
     :type type :for for :id id
     :prefix-currency prefix-currency
     :start-date start-date
     :end-date end-date
     :%where where :%fields fields :risk-type risk-type)))


