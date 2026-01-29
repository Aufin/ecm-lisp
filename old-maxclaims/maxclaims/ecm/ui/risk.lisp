(uiop:define-package :ecm/ui/risk
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.| #:$.)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/ui/utility
		#:cat #:<link-to-viewer> #:<item> #:corpus-name)
  (:import-from :ecm/user)
  (:import-from :ecm/entity/risk)
  (:import-from :ecm/json
		#:getjso #:getjso*)
  (:import-from :ecm/ui/navbar)
  (:import-from :ecm/ui/policy #:<policy>)
  (:import-from :ecm/ui/contract #:<contract>
		#:<select-contracts>)
  ;; (:import-from :ecm/endpoint)
  ;; (:import-from :ecm/entity/user)
  ;; (:import-from :ecm/entity/claim)
  ;; (:import-from :ecm/entity/attachment)
  ;; (:import-from :ecm/entity/corpus
  ;; 		#:corpus-name-as-string)
  ;; (:import-from :ecm/entity/timecard)
  ;; (:import-from :ecm/ui/attachment)
  (:export #:<claim-risk>
	   #:<select-risk-type>))

(in-package :ecm/ui/risk)

(defun ps/find-risk-object (&optional (term 'request.term))
  `({} 
     :_type "risk"
     :limit 10
     :where ({} "$or"
		({} :contract_number 
		    ({} "$ilike" (concatenate 'string ,term "%"))
		    :contract_number 
		    ({} "$ilike" (concatenate 'string "%",term "%"))
		    :policy_number 
		    ({} "$ilike" (concatenate 'string ,term "%"))
		    :insured_name 
		    ({} "$ilike" (concatenate 'string ,term "%"))))
     :order_by (ps:array
		({} "risk_id" ({} "$desc" t)))))

(defun <risk-code> (code &key (header "Code ")
			   (style "color:black")
			   (popover t)
			   (h "h3"))
  (when code
    (let* ((c (getjso "code" code))
	   (d (getjso "description" code))
	   (f (getjso "first_year_of_account" code))
	   (l (getjso "last_year_of_account" code))
	   (tr (getjso "terrorism_code" code))
	   (content (with-output-to-string (ecm/ml:*sexpml-output*)
		      (<> 'blockquote (<> :text d))
		      (<item> " first_year" f)
		      (when (not (equal 9999 l))
			(<item> " last_year" l))		      
		       (when tr
			 (<risk-code> tr :header "Terrorism Code"
				      :h "h5"
				      :popover nil))))
	   #+(or)(content (with-output-to-string (ecm/ml:*sexpml-output*)
		      (<> :text content))))		      
      (<> (div :class "row is-table-row align-baseline")
	(<> (div :class "col-3  text-xs-left")
	  (<> `(,h) (<link-to-viewer>
			("risk-code" (ecm/json:getjso "_id" code) :style style)
		      (<> :unescaped header))))
	(<> (div :class "col-9 ")
	  (<> (span :id (if popover "riskCodePopover" "")
		    :style "font-size:175%")
	    (<item> NIL c))))

      (when popover
	(<> (script)
	  (ps:ps*
	   `($.
	     "#riskCodePopover"
	     (popover ({} "html" t
			  "content" ,content
			  "trigger" "hover"))))))
      
				
				
      
      (<> (hr)))))


(defun <edit-risk-modal> (risk-id claim-id &key (clone nil))
  (<> (script)
    (ps:ps
      (defun refresh-risk ()
	(alert "refresh!"))
      (defvar risk ({})))
    (ps:ps* 
     `($ (lambda ()
	   (let* ((modal ($ "#ecmModal"))
		  (title ($. modal (find ".modal-title")))
		  (body ($. modal (find ".modal-body")))
		  (div ($ "#claimRisk")))

	     (defun show-edit-risk ()
	       (let ((iframe (+ "<iframe id=\"editRisk\" name=\"editRisk\" src=\"/ecm/risk/"
				,risk-id "/edit/inline\" style=\"border:none;width:100%;height:100%\"></iframe>")))
		 ($. title (empty)
		     (append (+
			      "<div style=\"width:100%\" class=\"text-xs-center\"> <h1> Update Risk </h1>"
			      (if ,clone
				  (+ "<a class=\"btn btn-primary\" href=\"/ecm/claim/",claim-id "/clone-risk\"> Clone Risk for claim #" , claim-id, "</a>")
				  "")
			      "</div>")))
		 ($. body (empty)
		     (append "<div id=\"editRiskIFrameLoading\" style=\"font-size:200%;width:100%\" class=\"text-xs-center\"><i  class=\"fa fa-spinner fa-spin spin-normal\"></i></div>") (append iframe))
		 ($. modal (modal "show"))
		 ($. body (find "iframe")
		     (on "load" (lambda ()
			     ($. body (find "#editRiskIFrameLoading") (hide)))))))

	     (defun hide-edit-risk ()
	       	 ($. title (empty) )
		 ($. body (empty) )
		 ($. modal (modal "hide")))

	     (defun update-risk ()
	       ($.get (+ "/ecm/claim/" ,claim-id "/risk")
		      (lambda (data)
			($. div (html data))
			($. "#claimRisk div" (effect "highlight" ({} "color" "#FFFF99") 3000)))))
	       

	     (setf risk ({} "show_edit" show-edit-risk
			    "hide_edit" hide-edit-risk
			    "risk_id" ,risk-id
			    "claim_id" ,claim-id
			    "update_risk" update-risk
			    "modal" modal
			    "title" title))))))))
      

(defun <claim-risk-edit> (&key show)
    (<> 'style
    ".ecm-c-header {
   color: black;
}

.edit-popup {
    position: absolute;
    top:0%;
    left: 90%;
    padding:10px;
    margin:0px;
    color:black;
    background-color:white;
    border: 1px solid lightgray;
    border-radius: 0.5rem;
    z-index: 10;
    -ms-filter: \"progid:DXImageTransform.Microsoft.Alpha(Opacity=70)\";       /* IE 8 */
    filter: alpha(opacity=70);  /* IE 5-7 */
    -moz-opacity: 0.7;          /* Netscape */
    -khtml-opacity: 0.7;        /* Safari 1.x */
    opacity: 0;               /* Good browsers */
}

.edit-popup:hover {
    -ms-filter: \"progid:DXImageTransform.Microsoft.Alpha(Opacity=100)\";       /* IE 8 */
    filter: alpha(opacity=100);  /* IE 5-7 */
    -moz-opacity: 1;          /* Netscape */
    -khtml-opacity: 1;        /* Safari 1.x */
    opacity: 1;               /* Good browsers */
}

/* for touchscreens, leave it up always */
.can-touch .edit-popup {
    -ms-filter: \"progid:DXImageTransform.Microsoft.Alpha(Opacity=100)\";       /* IE 8 */
    filter: alpha(opacity=100);  /* IE 5-7 */
    -moz-opacity: 1;          /* Netscape */
    -khtml-opacity: 1;        /* Safari 1.x */
    opacity: 1;               /* Good browsers */
}



")

  (<> (script)
    '|
$(function() {
 $('#claimRisk').each(
     function() { 
     $( this ).append( $( '<button class="edit-popup" data-toggle="tooltip" data-placement="bottom" title="Click to Edit Risk"> <i class="fa fa-pencil-square-o" aria-hidden="true"></i></button>'));

      $(this).find('.edit-popup').hover(
        function() {$(this).css('cursor','pointer');}, 
        function() {$(this).css('cursor','auto');})
       .click(function() {risk.show_edit();});
    }) ;

 
 $('#claimRisk').hover(
     function() { $( this ).find( ".edit-popup" ).css("opacity", "1"); },
     function() { $( this ).find( ".edit-popup" ).css("opacity", "0"); } 
  );



});
|)

  (when show  (<> (script)
    '|
$(function() {
       risk.show_edit(); 
});

|)))


(defun <claim-risk> (claim &key (show-edit nil))


  (let* ((risk (getjso "risk" claim))
	     (code (getjso "risk_code" risk))
	     (risk-id (getjso "_id" risk))
	     (risk (if (stringp code)
		           (ecm/entity/risk:find-risk-detail risk-id)
		         risk))
	     (code (getjso "risk_code" risk))
	     (policy (getjso "policy" risk))
	     (london-broker (getjso "london_broker" risk))
	     (contract (getjso "contract" risk))
	     (loss-fund-balance (getjso "loss_fund_balance" contract))
	     (sub (getjso "subscription_percent" claim))
         (num (getjso "risk_number" risk)))
    ;;(warn "Viewing claim risk: ~A" risk)
    (<> (div :id "claimRisk" :data-risk-id risk-id)
        (when (ecm/user:user-can-edit-p)
	      (<edit-risk-modal> (getjso "_id" risk) (getjso "_id" claim)
			                 :clone (> (length (getjso "claims" risk)) 1))
	      (<claim-risk-edit> :show show-edit))
        ;; ** Risk
        (<> (div :class "row is-table-row")
	        (<> (div :class "col-3  text-xs-left")
	            (<> 'h3 (<link-to-viewer>
		                 ("risk" (getjso "_id" risk) :style "color:black")
		                 (<> :text "Risk" ))))
	        (<> (div :class "col-9 ")
	            (<link-to-viewer>
	             ("risk" (getjso "_id" risk))
	             (<> :text "Type: " (getjso "risk_type" risk)))))
        ;; ** Policy
        (<> (div :class "row is-table-row")
	        (<> (div :class "col-3  text-xs-left align-baseline")
	            (<> 'h5 (<link-to-viewer>
		                 ("policy" (getjso "_id" policy) :style "color:gray")
		                 (<> :unescaped "&nbsp;Policy"))))
	        (<> (div :class "col-9  text-xs-left")
	            (<policy> policy)))
        ;; ** Contract
        (<> (div :class "row is-table-row")
	        (<> (div :class "col-3  text-xs-left align-baseline")
	            (<> (h5 :class "align-baseline")
	                (<link-to-viewer>
		             ("contract" (getjso "_id" contract) :style "color:gray")
	                 (<> :unescaped "&nbsp;Contract"))))
	        (<> (div :class "col-9  text-xs-left")
	            (<contract> contract)))

     ;(<> "LONDON BROKER GOES HERE")
     ; (<item> "London Browser" london-broker)
      (when london-broker
		(<> (div :class "row is-table-row")
		  (<> (div :class "col-3  text-xs-left align-baseline")
			(<> (h5 :class "align-baseline")
			  (<link-to-viewer>
				  ("person" (getjso "_id" london-broker) :style "color:gray")
				(<> :unescaped "&nbsp;London Broker"))))
		  (<> (div :class "col-9  text-xs-left")
			(<link-to-viewer>
				("person" (getjso "_id" london-broker))
		  
			  (<> :text (corpus-name london-broker))))))
	  ;; * Loss Fund Balance
	  (when loss-fund-balance
		(<> (div :class "row is-table-row")
		  (<> (div :class "col-3  text-xs-left align-baseline")
			(<> (h5 :class "align-baseline" :style "color:grey")
			  (<> :unescaped "&nbsp;Loss Fund")))
		  (<> (div :class "col-9  text-xs-left")
			(<> :text "$"(format nil "~,vf~%" 2 loss-fund-balance)))))
        ;; ** Risk COde

        (<risk-code> code
		             :style "color:gray"
		             :header "&nbsp;Code"
		             :h "h5")

        ;; ** Subscription %
        (when sub
	      (<> (div :class "row")
	          (<> (div :class "col-3  text-xs-left"
		               :style "color:gray")
	              (<> 'h6 (<> :unescaped "&nbsp;Subscription")))
	          (<> (div :class "col-9  text-xs-left")
	              (<> :text  (with-output-to-string (s)
			                                        (princ sub s)
			                                        (write-string "%" s))))))
        ;; * Risk Number
        (when num
          (<> (div :class "row")
	          (<> (div :class "col-3  text-xs-left"
		               :style "color:gray")
	              (<> 'h6 (<> :unescaped "&nbsp;Number")))
	          (<> (div :class "col-9  text-xs-left")
	              (<> :text  num))))
        (<> (hr)))))

(defun risk-page (risk &key (edit nil) (inline nil))
  (<> (ecm/ui/page:page :title "Risk")
    (unless inline
      (<risk-navbar> (getjso "_id" risk)
		     (getjso "risk_type" risk)
		     (getjso "policy" risk)))
    (<> 'hr)
    (<risk> risk :edit edit :inline inline)))

(defun <risk-navbar> (risk-id type policy)

  (<> (ecm/ui/navbar:navbar :type "risk" :id risk-id)         
    (<> 'html5:style
      " html, body {
    height: 100%;
 
}
body #navClaimTitle {
 text-align:center;
 text-overflow : ellipsis;
 white-space   : nowrap;
 position:fixed !important;
 position: absolute; /*ie6 */
 overflow:hidden ;
 top: 0.5rem; right:6rem; left:5.5em;
 
}

.ecm-nav { padding: 1em; }
.ecm-nav .dropdown-menu {
  top: 0.75rem; left: 0.25em;
}
")
      
    (<> (div :id "navClaimTitle")
      (<> 'h1
	(<> :unescaped type "&nbsp;Risk&nbsp;for&nbsp;")
	(<link-to-viewer>
	    ("policy" (getjso "_id" policy) :style "color:white")
	  (<> :text (getjso "policy_number" policy)))))))



(defun <select-risk-code> (&key selected-code (name "risk-code"))
  (let ((sc (when selected-code (getjso "code" selected-code)))
	(codes (ecm/entity/risk:active-risk-codes)))
    (<> (select :name name :class "form-control")
      (<> (option :value "") "None")
      (dolist (code codes)
	(let ((c (getjso "code" code))
	      (d (getjso "description" code)))
	  (<> (option :value c (when (equalp sc c) "selected"))
	    (<> :text c " " (first (split-sequence:split-sequence #\- d)))))))))

(defun <select-risk-type> (selected
			   &key
			     (style "")
			     (all-risks nil))
  (<> (select :name "risk-type"
	      :class "form-control ecm-select")
    (when all-risks
      (<> (option :style style :value "" (when t
					   "selected"))
	(<> "All Risk Types")))
    (dolist (r (ecm/entity/risk:risk-types))
      (<> (option :style style :value r (when (equal selected r) "selected"))
	(<> :text r)))))
	
(defun <chosen> ()
  	     (<> (style)
	       ".chosen-container {
     font-size: 1rem;

}
.chosen-container-single .chosen-single {
    display: block;
    width: 100%;
    padding: .5rem .75rem;
    padding-bottom: 1.75rem;
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

.chosen-container-single .chosen-single div b {
    margin-top: 0.4rem;
}

")
	     (<> 'html5:script
	       "$(function() { 

  $('select').chosen() ;
});
"))
  
(defun <risk> (risk &key (edit nil) (inline nil))

  (flet ((%risk () 
	       (let* ((policy (getjso "policy" risk))
		          (contract (getjso "contract" risk))
		          (code (getjso "risk_code" risk))
		          (type (getjso "risk_type" risk))
		          (claims (getjso "claims" risk))
                  (num (getjso "risk_number" risk)))
             ;; (warn "Risk: ~A" risk)
	         (when edit (<chosen>))
	         ;; ** Type
	         (<> (div :class "row is-table-row")
	             (<> (div :class "col-3  text-xs-left")
		             (<> 'h3 (<link-to-viewer>
			                  ("risk" (getjso "_id" risk) :style "color:black")
			                  (<> :text "Type" ))))
	             (<> (div :class "col-9 ")
		             (if edit
		                 (<select-risk-type> type)
		               (<> :text type))))
	         (<> 'hr)
	         ;; ** Policy
	         (<> (div :class "row is-table-row")
	             (<> (div :class "col-3  text-xs-left align-baseline")
		             (<> 'h3
		                 (<link-to-viewer>
		                  ("policy" (getjso "_id" policy) :style "color:black")
		                  (<> :unescaped "Policy"))))
	             (<> (div :class "col-9  text-xs-left")
		             (if edit
		                 (progn (ecm/ui/policy:<policy-input> policy))
		               (<policy> policy))))

	         (<> 'hr)
    
	         ;; ** Contract
	         (<> (div :class "row is-table-row")
	             (<> (div :class "col-3  text-xs-left align-baseline")
		             (<> (h3 :class "align-baseline")
		                 (<link-to-viewer>
		                  ("contract" (getjso "_id" contract) :style "color:black")
		                  (<> :unescaped "Contract"))))
	             (<> (div :class "col-9  text-xs-left")
		             (if edit
		                 (<select-contracts> :selected contract)
		               (<contract> contract))))
	         (<> 'hr)
	         ;; ** Code
	         (if edit
		         (<> (div :class "row is-table-row align-baseline")
		             (<> (div :class "col-3  text-xs-left")
		                 (if code
			                 (<> 'h3 (<link-to-viewer>
				                      ("risk-code" (ecm/json:getjso "_id" code) :style "color:black")
				                      (<> :text "Code")))
			               (<> 'h3 "Code")))
		             (<> (div :class "col-9 ")
		                 (<select-risk-code> :selected-code code))
		             (<> (hr)))
		       (<risk-code> code))

             ;; ** Lineage Number

	         (<> (hr))
             (<> (div :class "row is-table-row")
	             (<> (div :class "col-3  text-xs-left align-baseline")
		             (<> (h3 :class "align-baseline")
		                 (<> :text "Lineage Number")))
	             (<> (div :class "col-9  text-xs-left")
		             (if edit
		                 (<> `(input :type "text" :class "form-control"
                                     :name "number" :value ,num))
		               (<> :text (or num "N/A")))))
	         (<> (hr))
	         ;; ** Claims
	         (<> (div :class "row is-table-row align-baseline")
	             (<> (div :class "col-3  text-xs-left")
		             (<> 'h3 (<> :text "Claim" (if (rest claims) "s" ""))))
	             (<> (div :class "col-9")
		             (<> (ul :class "list-unstyled")
		                 (dolist (c claims)
		                   (<> (li :class "list-item")
		                       (<> (a :href (cat "/ecm/claim/" (getjso "_id" c)))
			                       (<> :text "#"(getjso "_id" c) ", " (getjso "status" c)
			                           ", Date of Loss: " (getjso "date_of_loss" c)
			                           ", Examiner: " (ecm/entity/corpus:corpus-name-as-string (getjso "examiner" c))))
		                       (when (ecm/user:user-is-administrator-p)
			                     (<> " ")
			                     (<> (a :href (cat "/ecm/delete?claim="(getjso "_id" c)
					                               "&access[type]=risk&access[id]="(getjso "_id" risk))
				                        :class "btn btn-secondary btn-sm")
			                         "Delete"))))
				  
		                 (when (and (ecm/user:user-can-edit-p) (not edit))
		                   (<> (li)
		                       (<> (a :class "btn btn-secondary btn-sm"
			                          :target "_blank"
			                          :href (cat "/ecm/create?create[type]=claim&create[key]=risk-id&access[type]=risk&access[id]="
					                             (getjso "_id" risk)"&back[active-tab]=Claims"))
			                       "Create New"))))))
	         (<> (hr))
	         (when edit
	           (<> (button :type "submit" :class "btn btn-success risk-edit-submit" :style "float:left")
		           "Update Risk")
	           (<> (a :id "cancelEditRisk"
		              :href (if inline "#" (cat "/ecm/risk/" (getjso "_id" risk)))
		              :class "btn btn-danger" :style "float:right")
		           "Cancel")
	           (when inline
		         (<> (script) '|$( "#cancelEditRisk" ).click(function( event ) {
  event.preventDefault();
  parent.risk.hide_edit();
});|))))))

;;; The page
        (if edit
	        (<> (form :method "POST"
		              :action (cat "/ecm/risk/" (getjso "_id" risk) "/edit" (when inline "/inline")))
	            (%risk))
	      (%risk))))

(defun inline-post-page ()
  (if t
      (<> (ecm/ui/page:page :title "Risk Updating")
	(<> (script)
	  (ps:ps
	    ($. parent (find "#editRiskIFrameLoading") (show))	    
	    (= (|.| parent location search) "")	    
	    (|.| parent location (reload)))))))
  
	 
	
