(defpackage :ecm/ui/report/mi
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
  (:import-from :ecm/report/mi #:syndicate-mi-report
		#:syndicate-mi-report-spreadsheet
		#:contract-mi-report
		#:contract-mi-report-spreadsheet)
  (:export :list-syndicates))
(in-package :ecm/ui/report/mi)

(hunchentoot:define-easy-handler (mi-report-handler
                                  :uri "/ecm/report/mi")
    ((syndicate-id :parameter-type 'integer)
     contract-id
     start-date
     end-date
     spreadsheet-type)
  (if (string-equal "POST" (hunchentoot:request-method*))
      (with-user ()
	(<download-spreadsheet>
	 (if (and contract-id (not (equalp contract-id "")))
	     (contract-mi-report-spreadsheet
	      contract-id
	      start-date
	      end-date)
	     (syndicate-mi-report-spreadsheet
	      syndicate-id
	      start-date
	      end-date))
	 :type spreadsheet-type))
			      
       
      (<> (ecm/ui/page:page :title "MI Report : ECM")
	(when (not (some (lambda (x)
			   (or (equalp x "")
			       (null x)))
			 (list syndicate-id
			       start-date
			       end-date)))
	  (<> :text (syndicate-mi-report  syndicate-id
					  start-date
					  end-date)))
	(<> (ecm/ui/navbar:navbar)
	        (<> 'html5:style
	" html, body {
    height: 100%;
}
html {
  font-size: calc(11px + (26 - 14) * ((100vw - 300px) / (1600 - 300)));
  background-color: lightgray;
  line-height: calc(1.3em + (1.5 - 1.2) * ((100vw - 300px)/(1600 - 300)));
}


@media screen and (min-width: 1000px) {
  html {
    font-size: 16px;
  }
}

@media screen and (max-width: 800px) {
  h1 {
    font-size: 2rem;
  }
}
@media screen and (max-width: 500px) {
  h1 {
    font-size: 1.75rem;
  }
}


.ui-autocomplete {
   position:relative;
}

")
		(<> '(div :id "navTitle")
		  (<> '(html5:h1 :class "pt-1") "Performance Management Information")))
	(<> 'br)
	(<mi-report-form> syndicate-id contract-id)
	        (<> 'style
          (<> "iframe {
    border: none;
    resize: both;
  overflow: auto;
}"))
        (<> '(iframe :name "report"  
              :id "report"
              :style "width:100%; overflow:scroll;")
          " ")

	(<iframe-load-resize> "#report"))))

(defun <mi-report-form> (syndicate-id contract-id)

  (<> (div :style "height:100%"))
  
  (<> '(form :method "POST"
	      :class "form-inline"
	      :action "/ecm/report/mi"
	      :target "report")
    
    (<> '(div :class "row mx-auto text-center h-100 w-100"
	        :id "contractShow"))

    (<> (div :class "row mx-auto w-100 text-center mb-3" :id "contractHide")
	      (<> '(div :class "col"
	            )
	        ;; Syndicate
	        (<> 'h3 (<> "Syndicate"))
	        (ecm/user:with-user ()
	          (<> '(select :name "syndicate-id" :class "form-control w-100"
		              :style "display:inline-block")
	            (dolist (s (list-syndicates))
		            (<> `(option :value ,(second s)
			                       ,@(when (equalp (princ-to-string (second s))
					                                   syndicate-id)
				                         (list :selected "selected")))
		              (<> :text (first s)))))))
        (<> (div :class "w-100 d-md-none"))
        (<> (div :class "col-md-1 mx-auto w-100"
                 :style "font-size:125%")
          (<> :text "or"))
        (<> (div :class "w-100 d-md-none"))
	      (ecm/user:with-user ()
	        #+(or)(ecm/ui/contract:<select-contracts> :selected contract-id
						                                        :allow-none t)
	        (<> '(div :class "col")
            (<> (input :type "hidden" :name "contract-id" :id "contractID"
		                   :value contract-id))
	          (<> 'h3 (<> " Contract "))
	          (<> (span :id "contractiHide" )
	            (<> '(input :id "contract" :class "form-control w-100"
                    :style "max-width: 100vw"
		                :placeholder "Enter Contract Number"))
	            (<> '(div :id "contractAutoAppend"
                :onload "this.width=$(\".container\").width();")))
	          (let ((contract (ecm/entity/contract:find-contract contract-id)))
	            (when contract
		            (ecm/ui/contract:<contract-display>
		             contract :style ""))
	            (when (and contract-id
			                   (not contract))
		            (<> (div :class "alert alert-danger"
			                   :id "contractInvalid")
		              (<> (strong) "Invalid Contract!")
		              (<> " A contract must be provided to continue"))))))
	      (ecm/ui/contract:<contract-autocomplete>
	       :display-style ""
	       :width "100%"))
    (<> 'br)
    
    (<> '(div :class "row mx-auto w-100 text-center")

      (<> 'html5:script
	      (ps:ps
	        ($ (lambda ()
	             ;;(|.| ($ ".report") (hide))
	             (ps:chain ($ ".datepicker")
			                   (datepicker))
	             (ps:chain ($ ".datepicker")
			                   (datepicker
			                    "option" "dateFormat" "yy-mm-dd")
			                   (datepicker
                          "option" "changeYear" t)
			                   (datepicker
                          "option" "changeMonth" t))
	             (let* ((date (ps:new (|Date|)))
		                  (month (ps:chain date (get-month)))
		                  (end-date (ps:new (|Date|))))
		             (|.| date (set-month 
			                      (if (= month 0) 
				                        11
				                        (ps:decf month))))
		             (|.| date (set-date 1))
		             (|.| end-date (set-date 1))
		             (when (= "" (|.| ($ "#start-date") (val)))
		               (ps:chain ($ "#start-date")
			                       (datepicker "setDate" date)))
		             (when (= "" (|.| ($ "#end-date") (val)))
		               (ps:chain ($ "#end-date")
			                       (datepicker "setDate" end-date))))))))
      (<> '(div :class "col")
        (<> 'h3 (<> "Start Date (including)")) 
	      (<> '(input :name "start-date"
		          :class "datepicker"
		          :id "start-date")))
      (<> '(div :class "col")
        (<> 'h3 (<> "End Date (excluding)")) 
	      (<> '(input :name "end-date"
		          :class "datepicker"
		          :id "end-date"))))
    
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
	        :type "hidden"))))

(defun list-syndicates ()
  (postmodern:query "
SELECT * 
 FROM (SELECT person_name(person) AS name, person_id 
       FROM person
       WHERE person_id IN (SELECT DISTINCT syndicate_id FROM contract)) 
 AS c
 ORDER BY name"))



