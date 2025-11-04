(defpackage :ecm/ui/claim/edit
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.|)
  (:import-from :ecm/api/find)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/ui/risk
		#:<risk>)
  (:import-from :ecm/ui/tabs)
  (:import-from :ecm/ui/iframe)
  (:import-from :ecm/ui/persistent-header)
  (:import-from :ecm/local-time)
  (:import-from :ecm/entity/ui)
  (:import-from :ecm/entity/claim)
  (:import-from :ecm/endpoint/attachment)
  (:import-from :ecm/ui/utility #:cat)
  (:import-from :ecm/user
		#:user #:user-id)
  (:import-from :ecm/json
		#:getjso
		#:getjso*)
  (:import-from :ecm/ui/claim/navbar
		#:<claim-navbar>)
  (:export #:<claim-edit-page>))
(in-package  :ecm/ui/claim/edit)

(defun <claim-datepicker> (&key (date nil) (name "date-of-loss"))
  (<> (input :type "text" :name name
	           :class "datepicker form-control"
	           :size 26
	           :value (or date "")))
  (<> 'html5:script
    (ps:ps
      ($(lambda ()
	        (ps:chain
	         ($ ".datepicker")
	         (datepicker
	          ({} "changeMonth" t
		            "orientation" "bottom"
		            "showTimezone" t

		            "dateFormat" "yy-mm-dd"
		            "maxDate"  0
		            "controlType" "select"
		            "oneLine" t))))))))

			(ecm/ui/utility:format-timestring
			 (ecm/local-time:format-rfc3339-timestring
			  t (ecm/local-time:universal-to-timestamp
			     (get-universal-time))))

(defun <claim-datetimepicker> (&key (date nil) (name "claim-received-time"))
  (<> (input :type "text" :name name
	           :class "datetimepicker form-control"
	           :size 26
	           :value (if date
			                  (ecm/ui/utility:format-timestring date)
			                  "")))
  (<> 'html5:script
    (ps:ps
      ($(lambda ()
	        (ps:chain
	         ($ ".datetimepicker")
	         (datetimepicker
	          ({} "changeMonth" t
		            "orientation" "bottom"
		            "showTimezone" t
		            "timeFormat" "HH:mm:ss Z"
		            "timeInput" t
		            "dateFormat" "D, d M yy"
		            "maxDate"  0
		            "controlType" "select"
		            "oneLine" t))))))))
(defun <select-label> (selected)
  (let ((lbls '("" "standard" "complex" "specialty")))
    (<> (select :class "form-control" :name "label")
      (dolist (lbl lbls)
	(<> (option :value lbl
		    (when (equalp lbl selected) "selected"))
	  (<> :text lbl))))))
(defun <select-industry-codes> (class industry)
  (<> (select :name "industry-code"
	      :class "form-control")
    (when (equalp class "")
      (setf class " "))
    (<> (option) "")
    (dolist (c (ecm/entity/claim:list-industry-codes))
      (ecm/json:mapjso (lambda (k v &aux (k (remove #\" k)))
			 (declare (ignore v))
			 (when (equalp k "")
			   (setf k " "))
			 (<> (option :value k
				     (when (and class
						(equalp k (remove
							   #\"
							   class)))
				       "selected"))
			   (<> :text (remove #\" k))))
		       c)))
    (dolist (c (ecm/entity/claim:list-industry-codes))
      (ecm/json:mapjso (lambda (k v &aux (k (remove #\" k)))
			 (when (equalp k "")
			   (setf k " "))
			 (<> (select :data-ind (if (equalp k "")
						   " "
						   k)
				     :name (cat "ind" k)
				     :style (unless (equalp k class)
					      "display:none")
				     :class "form-control")
			   (dolist (c v)
			     (<> (option :value (getjso "industry" c)
					 (when (equalp (getjso "industry" c)
						       industry)
					   "selected"))
			       (<> :text (cat (getjso "industry" c) " "
					      (remove #\" (getjso "description" c))))))))
		       c))
    (<> (script)
      '|
$(function ()
  { var ind; |
      (if (not class)
	  ""
	  (concatenate 'string
		       (string '| ind =  $('[data-ind="' +|)
		       (with-output-to-string (s) (print class s))
		       (string '| + '"]');|)))
      '|
    $('select[name=industry-code]').change(function () 
       { $(ind).hide(); 
         ind = $('[data-ind="' + $(this).val() + '"]'); 
       $(ind).show(); 
       });
  }); |

    ))

(defun <dollar-input> (&key value name
			 (placeholder "0.00"))
  (<> (div :class "input-group")
    (<>(div :class "input-group-addon")"$")
    (<> (input :type "text" :class "form-control" :placeholder placeholder
	       :name name
	       :value value))))

(defun <claim-edit-page>
    (claim-id
     &key (error nil)
       (claim (ecm/entity/claim:find-claim-crux claim-id ))
       (risk (ecm/entity/risk:find-risk-detail
	            (getjso "_id" (getjso "risk" claim))))
       (policy (getjso "policy" risk))
       (status (getjso "status" claim))
       (label (getjso "label" claim))
       (insured (getjso "insured" policy))
       (contract (getjso "contract" risk))
       (examiner (getjso "examiner" claim))
       (lineage (getjso "lineage" claim))
       (date-of-loss (getjso "date_of_loss" claim))
       (date-claim-made (getjso* "status_detail.date_claim_made" claim))
       (received (getjso* "status_detail.claim_received_time" claim))
       (acknowledged (getjso* "status_detail.claim_acknowledged_time" claim))
       (contacted (getjso* "status_detail.insured_contacted_time" claim))
       (first-site (getjso* "status_detail.first_site_visit_time" claim))
       (deductible (getjso* "balance.deductible" claim))
       (external-adjuster (getjso "external_adjuster" claim))
       (claimant (getjso "claimant" claim))
       (cause (getjso "cause" claim))
       (claim-authority (getjso* "authority.claim_authority" claim))
       (contract-authority (getjso "authority" contract))
       (peer-reviewed-date (getjso* "status_detail.peer_reviewed_date" claim))
       (recovery-date (getjso* "status_detail.recovery_subrogation_date" claim))
       (denial (ecm/json:from-json-bool (getjso* "status_detail.denial" claim)))
       (date-of-denial  (getjso* "status_detail.date_of_denial" claim))
       (reason-for-denial (getjso* "status_detail.reason_for_denial" claim))
       (complaint (getjso "complaint" claim))
       (date-of-complaint (and complaint (getjso "date" complaint)))
       (reason-for-complaint (and complaint (getjso "reason" complaint)))
       (refer-to-underwriters (ecm/json:from-json-bool (getjso* "status_detail.refer_to_underwriters" claim)))
       (open-for-recovery (ecm/json:from-json-bool (getjso* "status_detail.open_for_recovery" claim)))
	 
       (subscription
	      (getjso* "subscription_percent" claim))
       (coverage-counsel (getjso "coverage_counsel" claim))
       (defense-counsel (getjso "defense_counsel" claim))
       (restoration-firm-emergency (getjso "restoration_firm_emergency" claim))
       (restoration-firm-repair (getjso "restoration_firm_repair" claim))
       (line-of-business (getjso "line_of_business" claim))
       (coverage (getjso "coverage" claim))
       (industry-code (getjso "industry" claim))
       (industry-class (when industry-code
			                   (getjso "industry_classification" industry-code)))
       (industry (when industry-code
		               (getjso "industry" industry-code))))
                                        ;  (error "~A ~A" contacted (getjso "status_detail" claim))
  (<> (ecm/ui/page:page :title "Edit Claim")
    (let* ()
      ;(<> :text (st-json:write-json-to-string claim))
      #+(or)(<> :text
        (getjso "status_detail" claim))
      (<claim-navbar> claim-id insured status)
      (when error
	      (<> (p :class "text-danger")
	        (<> (pre :class "text-danger")
	          (<> :text (princ-to-string error)))))
00                                        ;(<modal>)
      (<style>)
      (<> (form :method "post"
		            :action "")
        (<> (div :class "row")
	        (<> (div :class "col-md-2")
	          "Status:")
	        (<> (div :class "col-md-4")
	          (let ((statuses '("Open" "Closed")))
	            (<> (select :name "claim-status" :class "form-control")
	              (dolist (s statuses)
		              (<> (option :value s (when (equalp status s) '(:selected t)))
		                (<> :text s))))))
	        (<> (div :class "col-md-2")
	          "Examiner:")
	        (<> (div :class "col-md-4")
	          (<select-examiner> examiner))
        
   (<> (div :class "col-md-2")
	          "Deductible")
	        (<> (div :class "col-md-2")
	          (<dollar-input>
	           :name "deductible"
	           :value deductible))
          (<> (div :class "col-md-2")
	          "Lineage:")
	        (<> (div :class "col-md-4")
            (<> (input :type "text"
                                        ; :size 3
                                        ; :style "max-width:3em;float:right"
			                 :class "form-control" 
			                 :name "lineage"
			                 :value lineage))))
	(<> (div :class "row")


	  (<> (div :class "col-md-2") "Label:")
	  (<> (div :class "col-md")
	    (<select-label> label))
        )
        (<> (div :class "row")
	        (<> (div :class "col-md-2")
	          "Date of Loss:")	
	        (<> (div :class "col-md-2")
	          (<claim-datepicker> :date date-of-loss :name "date-of-loss"))
          (<> (div :class "col-md-2")
	          "Date Claim Made:")
	        (<> (div :class "col-md-2")
	          (<claim-datepicker>
             :date date-claim-made :name "date-claim-made")))
	    
        (<> (div :class "row")

         (<> (div :class "col-md-2 ")
	          "Received Time:")
	        (<> (div :class "col-md-4")
	          (<claim-datetimepicker> :date received :name "received-time"))`
	        (<> (div :class "col-md-2")
	          "Acknowledged Time:")
	        (<> (div :class "col-md-4")
	          (<claim-datetimepicker>
	           :date acknowledged
	           :name "acknowledged-time")))

        (<> (div :class "row")
          (<> (div :class "col-md-2")
	          "Insured Contacted Time:")		  
	        (<> (div :class "col-md-4")
	          (<claim-datetimepicker>
	           :date contacted
	           :name "insured-contacted-time"))
	        (<> (div :class "col-md-2")
	          "First Site Visit Time:")		  
	        (<> (div :class "col-md-4")
	          (<claim-datetimepicker>
	           :date first-site
	           :name "first-site-visit-time")))
      
        (<> (div :class "row")
	        (<> (div :class "col-md-2")
	          "External Adjuster:")
	        (<> (div :class "col-md-8")
	          (ecm/ui/corpus:<corpus-input>
	           external-adjuster
	           :name "external-adjuster-id"
	           :prefix "external-adjuster-")))

        (<> (div :class "row")
	        (<> (div :class "col-md-2")
	          "Claimant / Plaintiff :")
	        (<> (div :class "col-md-8")
	          (ecm/ui/corpus:<corpus-input>
	           claimant
	           :name "claimant-id"
	           :prefix "claimant-")))
      
        (<> (div :class "row")
	        (<> (div :class "col-md-2")
	          "Cause Code:")
	        (<> (div :class "col-md-4")
	          (<select-cause> cause))
	        (<> (div :class "col-md-2")
	          "Industry Code")
	        (<> (div :class "col-md-4")
	          (<select-industry-codes> industry-class industry)))

        (<> (div :class "row")
	        (<> (div :class "col-md-2")
	          "Claim Authority")	
	        (<> (div :class "col-md-2")
	          (<dollar-input> :name "authority"
			                      :value claim-authority
			                      :placeholder (or contract-authority "0.00")))
	        (<> (div :class "col-md-2")
	          "Peer Reviewed:")
	        (<> (div :class "col-md-2")
	          (<claim-datepicker> :date peer-reviewed-date
			                          :name "peer-reviewed"))
0	        (<> (div :class "col-md-2")
	          "Recovery Subrogation:")
	        (<> (div :class "col-md-2")
	          (<claim-datepicker> :date recovery-date
			                          :name "recovery-date")))
        (<> (div :class "row")
          (<> (div :class "col-md-2")
	          "Denial")
	        (<> (div :class "col-md-1")
	          (<> (input :type "checkbox" :name "denial"
		                   (when denial "checked"))))
          (<> (div :class "col-md-2 ")
	          "Date of Denial:")
	        (<> (div :class "col-md-4")
	          (<claim-datetimepicker>
             :date date-of-denial
             :name "date-of-denial"))
          (<> (input :type "text" :class "form-control"
                     :placeholder "Reason for Denial"
                     :style "margin: 1em; margin-top: 0.5rem;margin-bottom: 0.25rem"
	       :name "reason-for-denial"
	          :value reason-for-denial))

	  )
	
        (<> (div :class "row")
	  (<> (div :class "col-md-2") "Complaint? " (<> :unescaped "&nbsp;")
	    (<> (input :type "checkbox" :name "complaint" (when complaint "checked"))))
	  (<> (div :class "col-md-2 complaint ")
	    "Date of Complaint:")
	  (<> (div :class "col-md-4 complaint ")
	    (<claim-datetimepicker>
             :date date-of-complaint
             :name "date-of-complaint")))
	(<> (div :class "col-md-12 complaint ")
	  (<> (input :type "text" :class "form-control"
                     :placeholder "Reason for Complaint"
                     :style "margin: 1em; margin-top: 0.5rem;margin-bottom: 0.25rem"
		     :name "reason-for-complaint"
	             :value reason-for-complaint))
	  (<> (html5:script)
	    "
             ECM = typeof ECM == 'undefined' ? {} : ECM
             ECM.toggleComplaint = bool => {
                 let check = document.querySelector('input[name=complaint]'),
                     disp = boool => boool ? 'block' : 'none',
                     bol = typeof bool === 'undefined' ? check.checked : bool
               console.log('Toole', disp(bol))

              document.querySelectorAll('.complaint').forEach( n => n.style.display = disp(bol) )
             }


             $(function () {
              ECM.toggleComplaint()
              document.querySelector('input[name=complaint]').addEventListener('click', e =>{
               ECM.toggleComplaint()
              })

            }

      )")
	  )




	
        (<> (div :class "row")
	        (<> (div :class "col-md-1")
	          "Subscription:")	
	        (<> (div :class "col-md-2")
	          (<> (div :class "input-group")
	            (<> (input :type "text"
			                   :size 3
			                   :style "max-width:3em;float:right"
			                   :class "form-control" 
			                   :name "subscription"
			                   :value subscription))
	            (<>(div :class "input-group-addon")"%")))

	 
	  
      
	        (<> (div :class "col-md-2")
	          "Refer to Underwriters")
	        (<> (div :class "col-md-1")
	          (<> (input :type "checkbox" :name "refer-to-underwriters"
		                   (when refer-to-underwriters "checked"))))
	        (<> (div :class "col-md-2")
	          "Open for Recovery")
	        (<> (div :class "col-md-1")
	          (<> (input :type "checkbox" :name "open-for-recovery"
		                   (when open-for-recovery "checked")))))

        ;; ** Restoration Firms
        (<> (div :class "row")
	        (<> (div :class "col-md-2")
	          "Restoration Firm (emergency):")
	        (<> (div :class "col-md-8")
	          (ecm/ui/corpus:<corpus-input>
	           restoration-firm-emergency
	           :name "restoration-firm-emergency-id"
	           :prefix "restoration-firm-emergency-")))

        (<> (div :class "row")
	        (<> (div :class "col-md-2")
	          "Restoration Firm (repair):")
	        (<> (div :class "col-md-8")
	          (ecm/ui/corpus:<corpus-input>
	           restoration-firm-repair
	           :name "restoration-firm-repair-id"
	           :prefix "restoration-firm-repair-")))
        ;; ** Coverage Counsel
        (<> (div :class "row")
	        (<> (div :class "col-md-2")
	          "Coverage Counsel:")
	        (<> (div :class "col-md-8")
	          (ecm/ui/corpus:<corpus-input>
	           coverage-counsel
	           :name "coverage-counsel-id"
	           :prefix "coverage-counsel-")))

          
        (<> (div :class "row")
	        (<> (div :class "col-md-2")
	          "Defense Counsel:")
	        (<> (div :class "col-md-8")
	          (ecm/ui/corpus:<corpus-input>
	           defense-counsel
	           :name "defense-counsel-id"
	           :prefix "defense-counsel-")))

        (<> (div :class "row")
	        (<> (div :class "col-md-3")
	          "Line of Business:")
	        (<> (div :class "col-md-3")
	          (<> (select :class "form-control"
		                    :name "line-of-business")
	            (<> (option) "")
	            (dolist (lob (ecm/entity/claim:list-line-of-businesses))
	              (<> (option :value lob
			                      (when (equalp line-of-business lob)
			                        "selected"))
		              (<> :text lob)))))
	        (<> (div :class "col-md-3")
	          "Coverage:")
	        (<> (div :class "col-md-3")
	          (<> (select :class "form-control"
		                    :name "coverage")
	            (<> (option) "")
	            (dolist (c (ecm/entity/claim:list-coverages))
	              (<> (option :value c
			                      (when (equalp coverage c)
			                        "selected"))
		              (<> :text c)))))
          (<> (button :type "submit" :class "btn btn-success risk-edit-submit" :style "float:left")
	          "Update Claim")
          (<> (a :id "cancelEditRisk"
	               :href (cat "/ecm/claim/" (getjso "_id" claim))
	               :class "btn btn-danger" :style "float:right")
	
	          "Cancel"))))))


(defun <select-cause> (selected)
  (<> (select :name "cause" :class "form-control")
    (<> (option) "")
    (when selected
      (<> (option :value (getjso "description" selected)
		  :selected t)
	(<> (:text (getjso "code" selected)
		   " " (getjso "description" selected)))))
    (dolist (e (ecm/entity/claim:list-claim-causes))
      (<> (option :value (getjso "description" e))
	(<> (:text (getjso "code" e) " " (getjso "description" e)))))))
  
(defun <select-examiner> (selected)
  (<> (select :name "examiner-id" :class "form-control")
    (when selected
      (<> (option :value (getjso "_id" selected))
	(<> (:text (getjso "full_name" selected)))))
    (dolist (e (ecm/entity/claim:list-examiners))
      (<> (option :value (getjso "_id" e))
	(<> (:text (getjso "full_name" e)))))))
		  
(defun <script-claim> ()
  ;;   $('.ecm-window-body').collapse('toggle') ;
   (<> '(html5:script)
	    '|
$(function () { 
  $(".ecm-window-minimize").click(function () 
    { 
      var target= $(this).attr("data-target");
      var href= $(this).attr("href") ;
      var where = target \|\| href;

     $(this).parents(".card-header").find("i.fa").toggleClass('fa-caret-square-o-up fa-caret-square-o-down');
    });
});|))

(defun <style> ()
    (<> 'style
      (<> :text
	;.corpus-delete { position:relative ; top:-1em; } 
      "
 .row {
   padding-bottom:5px;
   padding-top:5px;
   border-bottom:1px dotted lightgrey;
}

.collapse-hover {
    position: relative;
  }
  
  .hover-btn {
     float:right;
     position: absolute;
     right: -2em;

     z-index : 10;
      display: block;
    }
    
  
  .hover-timecard { 
    opacity: 0.5;
  }

  .hover-timecard:hover { 
    opacity: 1;
  }
     
    .collapse-hover:hover .hover-btn {

          display: block;
        }
  .ui-datepicker {

  z-index: 1060;

}

.ui-datepicker{z-index: 1060 !important};

")))
(defun <modal> ()
  (<> (div :class "modal fade" :id "ecmModal" :tabindex "-1" :role "dialog" :aria-hidden "true")
      (<> (div :class "modal-dialog modal-lg"
	       :style "width:90%;height:90%;max-width:none;z-index:10000")
	(<> (div :class "modal-content"
		 :style "height:100%;max-height:none;")
	  (<> (div :class "modal-header")
	    (<> (button :type "button" :class "close" :data-dismiss "modal" :aria-label "Close")
	      (<> (span :aria-hidden "true")
		(<> :unescaped "&times;")))
	    (<> (h4 :class "modal-title")))
	  (<> (div :class "modal-body"
		   :style "height:90%;;")
	    (<> "..."))))))
