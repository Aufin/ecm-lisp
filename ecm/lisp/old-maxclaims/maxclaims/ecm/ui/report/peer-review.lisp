(defpackage :ecm/ui/report/peer-review
  (:use :cl)
  (:import-from :ecm/ui/iframe #:<iframe-load-resize>)
  (:import-from :ecm/ui/contract)
  (:import-from :ecm/user #:with-user)
  (:import-from :ecm/ui/navbar)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.|)
  (:import-from :ecm/ui/spreadsheet
		#:<spreadsheet-type-select>
		#:<download-spreadsheet>)
  (:import-from :ecm/report/peer-review
		            #:peer-review-spreadsheet)
  (:export #:peer-review-page))
(in-package :ecm/ui/report/peer-review)

(defun peer-review-page ()
  (<> (ecm/ui/page:page :title "Peer Review Report : ECM")
    (<peer-report-form>)))

(defun <peer-report-form> ()
  (<> (div :style "height:100%")
    (<> '(form :method "POST"
	        :class "form-inline"
	        :action "/ecm/report/peer-review"
	        :target "report")
      (<> 'h1 "Peer Review Outstanding") 
      (<> '(div :class "row mx-auto text-center report border-top border-bottom my-3 w-100 py-3")
        (<> 'br)
        (<> '(div :class "col mc-auto")
	        (<> '(span :id "ss-type-span"
	              :class "row-centered"))
	        (<> '(button :type "submit" :class "btn btn-success")
	          (<> "Run Report"))
	        (<> " as ")
	        (<spreadsheet-type-select> 
	         :name "spreadsheet-type"
	         :class "spreadsheet-type-select")))
      (<> 'hr)
      (<> '(input :name "present[SPREADSHEET-TYPE]"
	          :value "Gnumeric_Excel:xlsx2"
	          :type "hidden")))))



