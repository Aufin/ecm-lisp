(defpackage :ecm/ui/report
  (:use :cl)
  (:import-from :ecm/user #:with-user)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.| #:$.)
  (:import-from :ecm/ui/navbar)
  (:import-from :ecm/ui/report/mi #:list-syndicates)
  (:import-from :ecm/ui/spreadsheet
		#:<spreadsheet-type-select>
		#:<download-spreadsheet>)

  (:export #:bordereau-page))
(in-package :ecm/ui/report)

(defun report-page ()
  (<> '(ecm/ui/page:page :title "Reports")
    (<report-navbar> "Reports")
    (<> (div :class "row")
      (unless (ecm/user:user-is-administrator-p)
	      (<> (div :class "col-md-2") ""))
      (<> (div :class "col-md-4")
	      (<> (h3) "Bordereau")
        (<> '(style) "
         .btn.btn-link::marker { color : black }
         .btn.btn-link { text-align: left ; display: list-item }
        ")
        (<> (div :class "btn-group-vertical")
          (<> (button :class "btn btn-link"
                      :style "display:list-item"
                      :onclick
                      "window.location = 'report/api/bordereau/';")
            "Bordereau API "
            (<> :unescaped "&nbsp;")
            (<> (div :class "alert alert-success float-right p-0") "new"))
          (<> :unescaped '|<div class="btn-group" role="group">
    <button id="btnGroupDrop1" type="button" class="btn btn-link dropdown-toggle" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
      Older Bordereau Spreadsheets
    </button>
    <div class="dropdown-menu" aria-labelledby="btnGroupDrop1">|)

	        (<> (ul :style "max-height: 10vh; overflow:scroll"
                  :data-toggle "tooltip" :title "Scroll for more")
	        (<> (li)
          (<> (a :class "btn btn-link"
                 :href "/ecm/report/bordereau/casualty-liability")
	          (<> "Casualty / Liability")))
          (<> (li)
            (<> (a :class "btn btn-link":href "/ecm/report/bordereau/lloyds-v5")
	            (<> "Lloyds v5")))
          (<> 'li (<> (a :class "btn btn-link":href "/ecm/report/bordereau/ascot")
	                  (<> "Ascot Lloyds v5 Denial")))
          (<> 'li  (<> (a :class "btn btn-link":href "/ecm/report/bordereau/hiscox")
	                   (<> "Hiscox Positional BDX")))
          (<> 'li  (<> (a :class "btn btn-link":href "/ecm/report/bordereau/enstar")
	                   (<> "Enstar Lloyds v5")))
 (<> (li) (<> (a :class "btn btn-link":href "/ecm/report/bordereau/zurich") (<> "Zurich Bordereau")))
	        (<> (li)
	          (<> (a :class "btn btn-link":href "/ecm/report/bordereau/hub-dale")
	            (<> "HUB / Dale bordereau")))
	        (<> (li)
	          (<> (a :class "btn btn-link":href "/ecm/report/bordereau/lloyds-claim")
		          (<> "Lloyds Claims Reporting")))
	        (<> (li)
	          (<> (a :class "btn btn-link":href "/ecm/report/bordereau/commonwell-lloyds")
		          (<> "Commonwell Lloyds Reporting")))
	        (<> (li)
	          (<> (a :class "btn btn-link":href "/ecm/report/bordereau/property")
	            (<> "Property Claims")))
	        (<> (li)
	          (<> (a :class "btn btn-link":href "/ecm/report/bordereau/inter-hannover")
	            (<> "Inter-Hannover")))
	        (<> (li)
	          (<> (a :class "btn btn-link":href "/ecm/report/bordereau/arch")
	            (<> "Arch")))
	        (<> (li)
	          (<> (a :class "btn btn-link":href "/ecm/report/bordereau/white-oak")
	            (<> "White Oak")))
            (<> (li)
	            (<> (a :class "btn btn-link":href "/ecm/report/agency-bordereau")
	              (<> "Agency"))))

          (<> :unescaped '|
    </div>
  </div>|)
          (<> (button :class "btn btn-link"
                      :style "display:list-item"
                      :onclick
                      "window.location = 'report/transaction-bordereau';")
            "Normal Transactional")
          (<> (button :class "btn btn-link"
                      :style "display:list-item"
                      :onclick
                      "window.location = 'report/temple-transaction-bordereau';")
            "Temple transactional")
          ))
      (<> (div :class "col-md-4")
      (unless (ecm/user:user-read-only-p)
	      (<> (h3) "Manage")
	      (<> (ul)
		(<> (li)
	              (<> (a :href "/ecm/report/api/static-claims")
	                  "Static Claims"))
             (<> (li)
	              (<> (a :href "/report/claim")
	                  "Generic Claims Report (open/closed/examiner)"))
            (<> (li)
	              (<> (a :href "/report/diary")
	                   "Diary Entries"))
            (<> (li)
	              (<> (a :href "/ecm/report/peer-review")
	                  "Peer Review Outstanding"))
	          (<> (li)
	              (<> (a :href (format nil "/ecm/report/examiner-claims/~A"
				                             (ecm/user:user-person-id)))
	                  "Your Claims"))
	          (<> (li)
	              (<> (a :href "/ecm/report/time-recorded")
	                  "Your Time Recorded"))
	          (<> (li)
	              (<> (a :href "/report/pillar3")
	                  "Pillar 3"))
	          (<> (li)
	              (<>(a :href "/report/pmi")
	                 "Performance Management Information (MI)"))
	          (<> (li)
	              (<> (a :href "create?create[type]=spreadsheet-payee-week&access[read-only]=false")
	                  "Payee Sanctions Report"))))
	      (<> (h3) "Cheque Register")
	      (<> (ul)
	        (<> (li)
	          (<> (a :href "/report/cheque-register")
	            "Agency or Contract"))))
      (when (ecm/user:user-is-administrator-p)
	      (<> (div :class "col-md-4")
	        (<> (h3) "Administrator")
	        (<> (ul)
	          (<> (li)
		          (<> (a :href "/ecm/create?create[type]=interim-report&access[read-only]=false")
		            "Interim Amounts"))
	          (<> (li)
	            (<> (a :href "create?create[type]=open-claim-report&access[read-only]=false")
		            "Open Claims"))

	          (<> (li)
		          (<> (a :href "/ecm/create?create[type]=time-recorded-report&access[read-only]=false")
		            "Timecards: Time Recorded"))
	          (<> (li)
		          (<> (a :href "/report/claim")
		            "Claims over Authority"))))))

    (<> (hr))
    #+(or) (<> (iframe :src "/ecm/reports"
		                   :style "width:100%"))))

(defun find-page-title (type)
  (or (cdr (assoc
	    type
	    '(("casualty-liability" . "Casuality/Liability Bordereau")
	      ("property" . "Property Bordereau")
	      ("hiscox" . "Hiscox Positional BDX")
	      ("lloyds-v5" . "Lloyds v5 Bordereau")
	      ("enstar" . "Enstar Lloyds v5 Bordereau")
	      ("ascot" . "Ascot Lloyds v5 Bordereau")
	      ("hub-dale" . "HUB / Dale bordereau")
	      ("lloyds-claim" . "Lloyds Claims Reporting (v4)")
	      ("white-oak" . "White Oak Bordereau")
	      ("inter-hannover" . "Inter-Hannover Bordereau")
	      ("arch" . "Arch Bordereau"))
	    :test #'string=))
      "Bordereau"))

(defun <bordereau-style> ()
  (<> (style)
    "
#contractAutoAppend .ui-autocomplete {
  position: relative;
  top: -0.24rem;
}

.ui-selectmenu-button {
    display: block;
    width: 100%;
    padding: .5rem .75rem;
    font-size: 1rem;
    line-height: 1.25;
    color: #55595c;
    background-color: #fff;
    background-image: none;
    -webkit-background-clip: padding-box;
    background-clip: padding-box;
    border: 1px solid rgba(0,0,0,.15);
    border-radius: .25rem;
}

.ui-selectmenu-button .ui-icon {
    position: absolute;
    margin-top: 0.1em;
    right: 1em;

}

.ui-selectmenu-menu .ui-menu-item {
 text-align:center;

}

.ui-selectmenu-text {
 margin:0;
 text-align:center;

}"))

(defun bordereau-page
    (type &key
			      (error nil)
			      (page-title (find-page-title type))
			      (navbar-title page-title)
			      (spreadsheet-type "Gnumeric_Excel:xlsx2")
			      (contract-id nil contract-id-provided?)
            (syndicate-id nil syndicate-id-provided?))
  (<> (ecm/ui/page:page :title page-title)
    (let ((contract (when contract-id
		                  (ecm/entity/contract:find-contract contract-id))))
      (<bordereau-style>)
      (<report-navbar> navbar-title)
      (when error
        (<> (div :class "alert alert-danger"
	               :style "max-height: 5em; overflow: auto;")
	        (<> (strong) "ERROR:")
	        (<> :text (format nil "~A" error))))
      (<> (br))

	       ;; (<> (strong) "Herenow")
      (<> (form :method "POST"
	              :target "_blank"
	              :action (concatenate 'string "/ecm/report/bordereau/" type))
        ;; (<> :text (ecm/hunchentoot:post-parameters*))
        (<> (input :type "hidden" :name "contract-id" :id "contractID"
		               :value contract-id))
        (<> '(div :class "row" :style "width:100%;text-align:center;")
          (<> '(div :class "col-md" :id "contractShow")
            (<> 'h3 (<> "Contract "))
	          (<> (span :id "contractHide" :style "margin-left:10px")
	            (<> '(input :id "contract" :class "form-control"
		                :style "display:inline-block"))
	            (<> '(div :id "contractAutoAppend"
		                :onload "this.width=$(\".container\").width();")))
	          (when contract
	            (ecm/ui/contract:<contract-display> contract))
	          (when (and (not contract)
		                   contract-id-provided?)
	            (<> (div :class "alert alert-danger"
		                   :id "contractInvalid")
	              (<> (strong) "Invalid Contract!")
	              (<> " A contract must be provided to continue"))))
          (when (member type '("lloyds-v5" "zurich" "enstar" "ascot" "hiscox") :test #'equalp)
            (<> '(div :class "col-md-1")
	            (<> 'h3 " or "))
            (<> '(div :class "col-md")

              (<> 'h3 (<> "Syndicate"))
	            (ecm/user:with-user ()
	              (<> '(select :name "syndicate-id" :class "form-control"
		                  :style "display:inline-block")
                  (<> (option :value "") "")
	                (dolist (s (list-syndicates))
		                (<> `(option :value ,(second s)
			                           ,@(when (equalp (princ-to-string (second s))
					                                       syndicate-id)
				                             (list :selected "selected")))
		                  (<> :text (first s)))))))))
        (<> '(div :class "row" :style "width:100%;text-align:center;")
          (<> '(div :class "col-md-4 col-centered")
            (<> 'h3 (<> "Start Date"))
            (<> '(input :name "start-date"
		              :style "text-align:center;"
                  :class "datepicker form-control"
                  :id "startDate")))
          (<> '(div :class "col-md-4 col-centered")
            (<> 'h3 (<> "End Date (excluding)"))
            (<> '(input :name "end-date"
		              :style "text-align:center;"
                  :class "datepicker form-control"
                  :id "endDate")))
          (<> '(div :class "col-md-4 col-centered")
            (<> 'h3 (<> "Risk Type"))
            (ecm/ui/risk:<select-risk-type>
	           nil :style "text-align:center"
	           :all-risks t)))
        (<> 'hr)
        (<> (div :class "row" :style "width:100%;text-align:center;")
          (<> (div :class "col-md-12 text-center")
	          (<> (div :style "width:100%" :class "text-center")
	            (<> '(button :type "submit"
	                  :class "btn btn-success"
	                  :name "new-window"
	                  :value "true")
	              (<> "Open Report in New Window")))
	          (<> (div :style "width:100%" :class "text-center")
	            (<> (h4) "or"))
	          (<> (div :class "row")
	            (<> (div :class "col-md-4")
	              (<> '(button :type "submit"
		                  :class "btn btn-primary"
		                  :name "spreadsheet"
		                  :value "true")
	                (<> "Download Spreadsheet")))
	            (<> (div :class "col-md-1")
	              (<> (h4 :style "display:inline-block;") "as"))
	            (<> (div :class "col-md-7")
	              (<spreadsheet-type-select>
	               :type spreadsheet-type
	               :class "form-control ecm-select"
	               :in-browser nil)))))
        (<> 'html5:script
          (ps:ps
            ($ (lambda ()
	               ($. ".ecm-select" (selectmenu
				                            ({} "width" "100%")))
                 (ps:chain ($ ".datepicker")
                           (datepicker))
                 (ps:chain ($ ".datepicker")
                           (datepicker
                            "option" "dateFormat" "yy-mm-dd"))
                 (let* ((date (ps:new (|Date|)))
                        (month (ps:chain date (get-month)))
                        (year (ps:chain date (get-full-year)))
                        )
		               (|.| date (set-date 1))
		               (when (= "" (|.| ($ "#endDate") (val)))
		                 (ps:chain ($ "#endDate")
			                         (datepicker "setDate" date)))
                   (|.| date  (set-month
                              (if (= month 0)
                                  11
                                  (ps:decf month))))
                    (|.| date (set-year
                              (if (= month 0)
                                  (decf year)
                                  year)))
                   (when (= "" (|.| ($ "#startDate") (val)))
                     (ps:chain ($ "#startDate")
                               (datepicker "setDate" date))))))))

        (ecm/ui/contract:<contract-autocomplete>
         )))))

(defun <report-navbar> (title)
  (<> (ecm/ui/navbar:navbar)
	  (<> (div :id "navClaimTitle")
	    (<> (h3) (<> :text title)))))
