(defpackage :ecm/ui/report/bordereau
  (:use :cl)
  (:import-from :ecm/user #:with-user)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/json #:getjso)
  (:import-from :ecm/ps #:{} #:|.|)

  (:import-from :ecm/ui/contract)
  (:import-from :ecm/entity/contract))
(in-package :ecm/ui/report/bordereau)

(defun bordereau-report-page (contract-id start-date end-date risk-type
			      report)
       ;;(error "~A" (hunchentoot:post-parameters*))
  (<> (ecm/ui/page:page
       :title "Bordereau")
    ;;(<> :text contract-id start-date end-date risk-type)
    (let ((contract (ecm/entity/contract:find-contract contract-id)))
      (when contract
      (<> (h1)
	    (<> :text "Bordereau for " (ignore-errors (getjso "contract_number" contract))))
    (<> (table :class "table")
      (<> (tr)
	(<> (th) "From:") (<> (td) (<> :text start-date))
	(<> (th) "To:") (<> (td) (<> :text end-date))
	(when risk-type(<> (th) "Risk Type:")
	      (<> (td) (<> :text risk-type))))
      (<> (tr)
	(<> (td :colspan (if risk-type 7 5))
	  (ecm/ui/contract:<contract-display> contract))))))
    (<> (table :class "table table-bordered")
      (<> (thead)
	(<> (tr :class "bg-info")
	  (loop for (heading . value ) :in (first report)
	     :do (<> `(th ,@(when (equalp value "???")
			      (list :style "display:none" "none")))
		   (<> :text heading)))))
      (<> (tbody)
	(loop :with hide for line in report
	   :do
	     (<> (tr)
	     (loop for (heading . value) :in line
		:do (<> `(td ,@(when (or (equalp value "???")
					 (member heading hide :test #'string=))
				 (pushnew heading hide :test #'string=)
				 (list :style "display:none" "none")))
		      (<> :text
			(typecase value
			  ((or simple-date:date
			       simple-date:timestamp)
			   (ecm/ui/utility:format-timestring
			    value
			    :format ecm/local-time:+rfc3339-format/date-only+))
			  (keyword
			   (if (eq :null value) "" value))
			  (t value)))))))))))
