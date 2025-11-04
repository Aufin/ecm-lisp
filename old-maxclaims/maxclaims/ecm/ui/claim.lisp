(uiop:define-package :ecm/ui/claim
    (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.|)
  (:import-from :ecm/api/find)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/ui/claim/clone)
  (:import-from :ecm/ui/risk)
  (:import-from :ecm/ui/tabs)
  (:import-from :ecm/ui/iframe)
  (:import-from :ecm/ui/persistent-header)
  (:import-from :ecm/local-time)
  (:import-from  :ecm/entity/ui)
  (:import-from  :ecm/entity/claim)
  (:import-from  :ecm/endpoint/attachment)
  (:import-from  :ecm/ui/utility #:cat)
  (:import-from :ecm/user
		        #:user #:user-id #:user-can-edit-p)
  (:import-from :ecm/user
		        #:user #:user-id)
  (:import-from :ecm/json
		        #:getjso)
  (:import-from :ecm/ui/navbar #:<claim-navbar>)
  (:import-from :ecm/ui/claim/balance
		        #:<claim-balance-card>)
  (:export #:<claim-navbar>
	       ))
(in-package  :ecm/ui/claim)

(defun <script-claim> ()
  ;;   $('.ecm-window-body').collapse('toggle') ;
  (<> :unescaped '|<script src="https://cdnjs.cloudflare.com/ajax/libs/FitText.js/1.2.0/jquery.fittext.min.js"></script>|)
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
});



|))


;; #navClaimTitle {
;; white-space:nowrap;
;; line-height:0;
;; text-align:center;
;; color:white;
;; }
;; #navClaimTitle .nav-insured small { display:none;}
;; #navClaimTitle .nav-insured {
;; display:block;
;; width:75vw; position:relative; right: 5px;
;; overflow:hidden;
;; text-overflow: ellipsis;
;; }

;; #navClaimTitle h3 {
;; margin: 0;
;; margin-left: 0.25em;
;; font-size: calc(19px + (26 - 14) * ((100vw - 300px) / (1600 - 300)));
;; }
(defun <style-claim> ()
  (<> 'html5:style
    "
.cheque {
   max-width : 60vw;
}

#claimAccordion {
   margin: auto;
   width:95%;
   min-width:90vw;

   margin-top: 0.25em;
   margin-bottom: 3em;
}

.modal {z-index: 10052;}

h1,h2,h3,h4,h5,h6 {white-space:nowrap;}

.persist-header .ecm-nav-buttons { display:none;}

.card-header.floatingHeader .ecm-nav-buttons {
  display:inline;
  position:absolute;
  background-color: grey;
  right:-2rem;
  bottom:-1em;
  opacity:1;
  z-index: 54321;
}

.card-header.floatingHeader .ecm-nav-buttons a {
  background-color: grey;
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

.collapse-hover {
    position: relative;
}

  .hover-btn {
     float:right;
     position: absolute;
     right: -1rem;

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

"))
(defmacro <link-to-viewer> ((type id &key style)
			                &body body)
  `(<>
       `(html5:a
	     :href ,(concatenate
		         'string "/ecm/view?" ,type"="
		         (princ-to-string , id))
	     :target "_blank"
	     ,,@(when style `(:style ,style)))
     ,@body))

(defmacro <window> (heading &body body)
  `(progn
     (<> '(html5:div :class "container-fluid")
	   (<> '(html5:div :class "row collapse-hover")
	     (<> '(div :class "bg-primary"
		       :style "padding:5px")
	       (<> '(html5:h6 :style "display:inline")
	         (<> 'html5:strong
		       ,heading)))
	     (<> '(html5:div :class "ecm-window-body collapse show")
	       ,@body)))))



(defun <item> (label data &key style unescaped)
  (let ((s style))
    (when data
      (<> '(span :style "display:inline-block")
	    ;; Label
	    (<> '(html5:small :class "text-muted")
	      (let ((label (with-output-to-string (s)
			             (map nil (lambda (c)
				                    (if (char= #\Space c)
					                    (write-string "&nbsp;" s)
					                    (write-char c s)))
			                  label))))
	        (<> :unescaped label " ")))
	    ;; Data
	    (<> '(span :style "display:inline-block")
	      (typecase data
	        (ecm/json:jso
	         (cond ((equal "corpus_summary" (getjso "_type" data))
		            (<link-to-viewer>
		                ("person" (getjso "_id" data)
				         :style s)
		              (<> :text (corpus-name data))))
		           (t (if unescaped
			              (<> :unescaped data)
			              (<> :text data)))))
	        (t (if unescaped
			       (<> :unescaped data)
			       (<> :text data)))))))))

(defun location->string (location)
  (let ((line1 (getjso "address_line_1" location))
	    (line2 (getjso "address_line_2" location))
	    (city (getjso "city" location))
	    (province (getjso "province" location))
	    (postal-code (getjso "postal_code" location))
	    (country (getjso "country" location)))
    (with-output-to-string (s)
      (when line1 (princ line1 s))
      (when (and line1 line2) (princ #\Newline s))
      (when line2 (princ line2 s))
      (when (and (or line1 line2) (or city province postal-code)) (princ #\Newline s))
      (when city (princ city s))
      (when (and city province) (princ ", " s))
      (when province (princ province s))
      (when (or city province) (princ " " s))
      (when postal-code (princ postal-code s))
      (when (or line1 line2 city province postal-code)
	    (princ #\Newline s))
      (when country (princ country s)))))

(defun <risk> (claim)
  (let ((show-edit (ecm/hunchentoot:parameter-or-nil "edit-risk")))
    (ecm/ui/risk:<claim-risk> claim :show-edit show-edit)))

(defun <loss> (claim)
  (flet ((getjso (key map)
	       (when map
	         (ecm/json:getjso key map))))
    (let* ((loss (getjso "loss" claim))
	       (date (getjso "date_of_loss" claim))
	       (description (getjso "description" loss))
	       (loss-code (getjso "loss_code" loss))
	       (location (getjso "location" loss))
	       (catastrophe (getjso "catastrophe" loss)))

      ;; ** Date of Loss
      (<> (div :class "row ")
	    (<> (div :class "col-4  text-xs-left")
	      (<> 'h3 (<> :text "Loss")))
	    (<> (div :class "col-8 text-xs-left")
	      (<> (strong)
	        (<> :text "Date of Loss: "))
	      (<> :text date)))

      ;; ** Loss Description
      (when description
	    (<> (div :class "row")
	      (<> (div :class "col-4 col-lg-3 text-xs-left")
	        (<> (h5 :style "color:gray")
	          (<> :unescaped "&nbsp;Description")))
	      (<> (div :class "col-8 col-lg-9 text-xs-left")
	        (<> '(html5:blockquote :class "blockquote") (<> :text description)))))

      ;; ** Catastrophe
      (when catastrophe
	    (<> (div :class "row")
	      (<> (div :class "col-5 col-lg-4 col-xl-3 text-xs-left")
	        (<> (h5 :style "color:gray")
	          (<> :unescaped "&nbsp;Catastrophe")))
	      (<> (div :class "col-7 col-lg-8 col-xl-9 text-xs-left")
	        (<> '(html5:blockquote :class "blockquote")
	          (<> 'html5:strong
		        (<> :text (getjso "name" catastrophe) " "))
	          (<item> "code" (getjso "code" catastrophe))))))

      ;; ** Loss Code
      (when loss-code
	    (<> (div :class "row")
	      (<> (div :class "col-3  text-xs-left")
	        (<> (h5 :style "color:gray")
	          (<> :unescaped "&nbsp;Code")))
	      (<> (div :class "col-9  text-xs-left")
	        (<> '(html5:blockquote :class "blockquote")
	          (<> 'html5:strong
	            (<> :text (getjso "description" loss-code) " "))
	          (<item> "code" (getjso "code" loss-code))))))

      ;; ** Locaton
      (when location
	    (<> (div :class "row")
	      (<> (div :class "col-3  text-xs-left")
	        (<> (h5 :style "color:gray")
	          (<> :unescaped "&nbsp;Location")))
	      (<> (div :class "col-9  text-xs-left")
	        (<> 'html5:pre
	          (<> :text (location->string location))))))
      (<> (hr)))))

(defun <industry> (claim)
  (flet ((getjso (key map)
	       (when map
	         (ecm/json:getjso key map))))
    (let* ((ind (getjso "industry" claim))
	       (code (getjso "industry" ind))
	       (class (getjso "industry_classification" ind))
           (desc (getjso "description" ind)))
      (when ind
        (<> (div :class "row is-table-row")
	      (<> (div :class "col-3  text-xs-left")
	        (<> (h3) "Industry" )))
        (<> (div :class "col-9 ")
	      (<> (div :class "row")
	        (<> (div :class "col-3  text-xs-left")
	          (<> (h5 :style "color:gray")
	            (<> :unescaped "&nbsp;Class")))
	        (<> (div :class "col-9  text-xs-left")
	          (<> 'html5:strong
	            (<> :text class))))
          (<> (div :class "row")
            (<> (div :class "col-3  text-xs-left")
	          (<> (h5 :style "color:gray")
	            (<> :unescaped "&nbsp;Description")))
            (<> (div :class "col-9  text-xs-left")
	          (<> '(html5:blockquote :class "blockquote")
	            (<> :text desc))))
          (<> (div :class "row")
	        (<> (div :class "col-3  text-xs-left")
	          (<> (h5 :style "color:gray")
	            (<> :unescaped "&nbsp;Code")))
	        (<> (div :class "col-9  text-xs-left")
	          (<> '(span :style "font-size: 175%")
	            (<> :text code)))))
        (<> (hr))))))


(defun <policy> (policy)
  (<> 'b
    (<link-to-viewer> ("policy" (getjso "_id" policy))
      (<> :text (getjso "policy_number" policy))))
  (<> (div :class "text-nowrap"
	       :style "display:inline-block")
    (<> '(html5:small :class "text-muted")
      (<> :unescaped "&nbsp;" " effective" "&nbsp;"))
    (<> :text (getjso "effective_date" policy)))
  (<> (div :class "text-nowrap"
	       :style "display:inline-block")
    (<> '(html5:small :class "text-muted")
      (<> :unescaped "&nbsp;" " expiry" "&nbsp;"))
    (<> :text (getjso "expiry_date" policy))))

(defun <contract> (contract)
  (<> 'b
    (<link-to-viewer> ("contract" (getjso "_id" contract))
      (<> :text (getjso "contract_number" contract))))
  (<> " ")
  (<item> "effective date" (getjso "effective_date" contract))
  (<> " ")
  (<item> "expiry_date" (getjso "expiry_date" contract))
  (<> " ")
  (<item> "agency" (getjso "agency" contract))
  (<> " ")
  (<item> "syndicate" (getjso "syndicate" contract)))

(defun <cause> (cause)
  (<> (div :class "row")
    (<> (div :class "col-3  text-xs-left")
      (<> (h3)
	    (<> :unescaped "Cause")))
    (<> (div :class "col-9  text-xs-left")
      (<> '(html5:blockquote :class "blockquote")
	    (<> 'html5:strong
	      (<> :text (getjso "description" cause) " "))
	    (<item> "code" (getjso "code" cause))))
    (<> (hr))))

(defun <claimant> (claimant)
  (<> (div :class "row")
    (<> (div :class "col-4  text-xs-left")
      (<> (h3)
	    (<> :unescaped "Claimant")))
    (<> (div :class "col-8")
      (<item> "" claimant ))))

(defun <restoration-firm> (firm &optional (title "Restoration Firm (emergency)"))
  (<> (div :class "row")
    (<> (div :class "col-4  text-xs-left")
      (<> (h5 :style "color:gray")
	    (<> :unescaped title)))
    (<> (div :class "col-8")
      (<item> "" firm ))))

(defun <defense-counsel> (firm)
  (<> (div :class "row")
    (<> (div :class "col-4  text-xs-left")
      (<> (h5 :style "color:gray")
	    (<> :unescaped "Defense Counsel")))
    (<> (div :class "col-8")
      (<item> "" firm ))))

(defun <coverage-counsel> (firm)
  (<> (div :class "row")
    (<> (div :class "col-4  text-xs-left")
      (<> (h5 :style "color:gray")
	    (<> :unescaped "Coverage Counsel")))
    (<> (div :class "col-8")
      (<item> "" firm ))))

(defun <status-detail> (status-detail)
  (let* ((status (getjso "status" status-detail))
	(open (getjso "open_date" status-detail))
	(date_claim_made (getjso "date_claim_made" status-detail))
	(close (getjso "close_date" status-detail))
	(reopen (getjso "reopen_date" status-detail))
	(rec (getjso "claim_received_time" status-detail))
	(ack (getjso "claim_acknowledged_time" status-detail))
	(peer-rev (getjso "peer_reviewed_date" status-detail))
	(recovery (getjso "recovery_subrogation_date" status-detail))
	(contacted (getjso "insured_contacted_time" status-detail))
	(first-site (getjso "first_site_visit_time" status-detail))
	(open-for-recovery (getjso "open_for_recovery" status-detail))
	(denial (getjso "denial" status-detail))
	(date-of-denial (getjso "date_of_denial" status-detail))
	(reason-for-denial (getjso "reason_for_denial" status-detail))
	(label (getjso "label" status-detail))
	(complaint (getjso "complaint" status-detail))
	(date-of-complaint (and complaint (getjso "date" complaint)))
	(reason-for-complaint (and complaint (getjso "reason" complaint)))
	(refer-to-underwriters (getjso "refer_to_underwriters" status-detail)))
    ;; ** Status
    (<> (div :class "row")
      (<> (div :class "col-6  text-xs-left")
	      (<> (h3)
	        (<> :unescaped "Status")))
      (<> (div :class "col-6  text-xs-left")
	      (<> '(html5:h3 :style "display: inline-block;color:gray")
	        (<> :text (string-downcase status)))
	      (<> :unescaped "&nbsp;" "&nbsp;")
	      (<> :text
	        (if (equalp status "Open")
	            (or reopen open)
	            close))))

    ;; ** label
    (when label ;; (or label t)
      (<> (div :class "row")
	      (<> (div :class "col-6 text-xs-left")
	        (<> (h5 :style "color:gray; padding-left:10px;")
	          (<> :unescaped "Label")))
	      (<> (div :class "col-6  text-xs-left")
	        (<> 'strong (<> :text (string-downcase label))))))

    ;; ** Refer to underwriters
    (when refer-to-underwriters
      (<> (div :class "row")
	      (<> (div :class "col-6 text-xs-left")
	        (<> (h5 :style "color:gray; padding-left:10px;")
	          (<> :unescaped "Refer to Underwriters")))
	      (<> (div :class "col-6  text-xs-left")
	        (<> 'strong (<> :text (string-downcase refer-to-underwriters))))))


    ;; ** Open for Recovery
    (when open-for-recovery
      (<> (div :class "row")
	      (<> (div :class "col-6 text-xs-left")
	        (<> (h5 :style "color:gray; padding-left:10px;")
	          (<> :unescaped "Open for Recovery")))
	      (<> (div :class "col-6  text-xs-left")
	        (<> 'strong (<> :text
			      (string-downcase open-for-recovery))))))
    (when complaint
      (<> (div :class "row")
	(<> (div :class "col-6 text-xs-left")
	  (<> (h5 :style "color:gray; padding-left:10px;")
	    (<> :unescaped "Complaint")))
	  (<> (div :class "col-6  text-xs-left")
	  (<> 'strong (<> :text
			date-of-complaint))
	    
	    (<> 'pre (<> :text
			  reason-for-complaint)))))
    ;; ** Denial
    (when denial
      (<> (div :class "row")
	(<> (div :class "col-6 text-xs-left")
	  (<> (h5 :style "color:gray; padding-left:10px;")
	    (<> :unescaped "Denial")))
	(<> (div :class "col-6  text-xs-left")
	  (<> 'strong (<> :text
			(string-downcase denial)))))
      (when reason-for-denial
        (<> (div :class "row")
	  (<> (div :class "col-6 text-xs-left")
	    (<> (h5 :style "color:gray; padding-left:10px;")
	      (<> :unescaped "Reason for Denial")))
	  (<> (div :class "col-6  text-xs-left")
	    (<> 'strong (<> :text
			  reason-for-denial))))))
         (<> 'hr)

    ;; ** Timeline
    (<> (div :class "row")
      (<> (div :class "col-3  text-xs-left")
	      (<> (h3)
	        (<> :unescaped "Timeline"))))
    (<> (div :class "row")

      ;; *** Open
      (when open
	      (<> (div :class "col-3  text-xs-left")
	        (<> (h6 :style "color:gray; display:inline-block") "Open"))
	      (<> (div :class "col-3  text-xs-left")
	        (<> :unescaped "&nbsp;" open)))
      ;; *** Date Claim Made
      (when date_claim_made
	      (<> (div :class "col-3  text-xs-left")
	        (<> (h6 :style "color:gray; display:inline-block") "Date Claim Made"))
	      (<> (div :class "col-3  text-xs-left")
	        (<> :unescaped "&nbsp;" date_claim_made)))

;;; *** Closed
      (when close
	      (<> (div :class "col-3  text-xs-left")
	        (<> (h6 :style "color:grey")
	          (<> :unescaped "Close")))
	      (<> (div :class "col-3  text-xs-left")
	        (<> :text close))))
    (<> (div :class "row")
;;; *** Reopen
      (when reopen
      	(<> (div :class "col-12  text-xs-center")
	        (<> (h6 :style "color:grey; display:inline-block")
	          (<> :unescaped "Reopen&nbsp;"))

	        (<> :text reopen))))

    (<> (div :class "row ")
      (when recovery
      	(<> (div :class "col-3  text-xs-left")
	        (<> (h6 :style "color:grey")
	          (<> :unescaped "Recovery")))
	      (<> (div :class "col-3  text-xs-left")
	        (<> :text recovery)))
      (when peer-rev
	      (<> (div :class "col-3  text-xs-left")
	        (<> (h6 :style "color:grey")
	          (<> :unescaped "Peer&nbsp;Reviewed")))
	      (<> (div :class "col-3  text-xs-left")
	        (<> :text peer-rev))))
; *** Date of Denial
    (when date-of-denial
      (<> (div :class "row")
	      (<> (div :class "col-3  text-xs-left")
	        (<> (h6 :style "color:gray; display:inline-block")
            "Date of Denial"))
	      (<> (div :class "col-9  text-xs-left")
	        (<> :unescaped "&nbsp;" date-of-denial))))




;;; *** Received
    (when rec
	    (<> (div :class "row")


	      (<> '(html5:div :class
		          "  col-12 offset-md-0 text-xs-left ")
	        (<> '(h6 :style "color:grey; display: inline-block")
	          (<> :unescaped "Received" "&nbsp;"))
	        (<> :unescaped (ecm/local-time:pretty-print-timestring
			                    rec :nbsp t)))))
    ;; *** Acknowledge
    (when ack
	    (<> (div :class "row")
	      (<> '(html5:div :class
		          "  col-12 offset-md-0 text-xs-left ")
	        (<> '(h6 :style "color:grey; display: inline-block")
	          (<> :unescaped "Acknowledged" "&nbsp;"))
	        (<> :unescaped (ecm/local-time:pretty-print-timestring
			                    ack :nbsp t)))))
;;; * Insured contacted
    (when contacted
	    (<> (div :class "row")
	      (<> '(html5:div :class
		          "  col-12 offset-md-0 text-xs-left ")
	        (<> '(h6 :style "color:grey; display: inline-block")
	          (<> :unescaped "Insured Contacted" "&nbsp;"))
	        (<> :unescaped (ecm/local-time:pretty-print-timestring
			                    contacted :nbsp t)))))
;;; first site visit
    (when first-site
	    (<> (div :class "row")
	      (<> '(html5:div :class
		          "  col-12 offset-md-0 text-xs-left ")
	        (<> '(h6 :style "color:grey; display: inline-block")
	          (<> :unescaped "First Site Visit" "&nbsp;"))
	        (<> :unescaped (ecm/local-time:pretty-print-timestring
			                    first-site :nbsp t)))))))


(defun <examine> (claim)
  (let ((examiner (getjso "examiner" claim))
        (lineage (getjso "lineage" claim))
	    (external-adjuster (getjso "external_adjuster" claim))
	    (lob (getjso "line_of_business" claim))
	    ;;  (crawford #+nil(ecm/entity/claim:claim-crawford-number (getjso "_id" claim)))
        )

    (<> '(html5:div :class "row ")
      (<> '(html5:div :class "col-4")
	    (<> '(html5:h3)
	      (<> "Examiner")))
      (<> '(html5:div :class "col-8")
	    (<> (span :class "h6")
	      (<item> "" examiner ))))


    (when external-adjuster
      (<> '(html5:div :class "row ")
	    (<> '(html5:div :class "col-4")
	      (<> '(html5:h5 :class "text-muted")
	        (<> 'html5:th (<> :unescaped "&nbsp" "Adjuster"))))
	    (<> '(html5:div :class "col-8")
	      (<link-to-viewer> ("person" (getjso "_id" external-adjuster))
	        (<> :text (corpus-name external-adjuster))))))
    (when lineage
      (<> '(html5:div :class "row ")
	    (<> '(html5:div :class "col-4")
	      (<> '(html5:h3)
	        (<> :unescaped "Lineage")))
	    (<> '(html5:div :class "col-8")
	      (<> :text lineage))))
    (when lob
      (<> '(html5:div :class "row ")
	    (<> '(html5:div :class "col-4")
	      (<> '(html5:h6 :class "text-muted")
	        (<> 'html5:th (<> :unescaped "&nbsp" "Line" "&nbsp" "of"  "&nbsp" "Business"))))
	    (<> '(html5:div :class "col-8")
	      (<> :text lob))))
    (<> (html5:hr))

    #+(or)(when crawford
            (<> '(html5:div :class "row ")
	          (<> '(html5:div :class "col-4")
	            (<> '(html5:h6 :class "text-muted")
	              (<> 'html5:th (<> :unescaped "&nbsp" "Crawford" "&nbsp" "Claim #"))))
	          (<> '(html5:div :class "col-8")
	            (<> :text crawford))))))

(defun <diary> (diary-entry)
  (let* ((schedule (getjso "schedule" diary-entry))
	     (deadline (getjso "deadline" diary-entry))
	     (_id (getjso "_id" diary-entry))
	     (claim-id (getjso "claim_id" diary-entry))
	     (user (user))
	     #+(or)(diary-user-id (ecm/json:getjso* "user._id" diary-entry)))
    (<window>
	    (progn
	      (<> '(html5:div :style "text-align:center")
	        (<> '(html5:span :class "alert alert-danger"
		          :style "float:left;")
	          (<> :text "Outstanding Diary Entry"))
	        (<> (span :style "color:white")
	          (<> :text "Schedule: " schedule " • Deadline:" deadline))))
      (<> '(html5:div :class "center-block"
	        :style "text-align:center; padding-top:2.5px;")
	    (<> '(html5:div :class "btn-group")
	      (<> `(html5:a :class "btn btn-secondary btn-sm"
		                :href ,(concatenate
			                    'string
			                    "/ecm/view?diary-entry="
			                    (princ-to-string _id)))
	        (<> "View"))
	      (<> `(html5:a :class "btn btn-secondary btn-sm"
		                :href ,(concatenate
			                    'string
			                    "/ecm/edit?diary-entry="
			                    (princ-to-string _id)
			                    "&future[processed]=T&access[type]=claim"
			                    "&access[id]=" (princ-to-string claim-id)
			                    "&go-back-to-claim="(princ-to-string claim-id)))
	        (<> "Mark as Done"))
	      (<> `(html5:a :class "btn btn-secondary btn-sm"
		                :href ,(concatenate
			                    'string
			                    "/ecm/create?create[type]=defer-diary-entry"
			                    "&create[key]=diary-entry-id"
			                    "&access[type]=diary-entry"
			                    "&access[id]=" (princ-to-string _id)
			                    "&go-back-to-claim="(princ-to-string claim-id)))
	        (<> "Defer"))
	      (when (ecm/user:user-is-administrator-p user)
	        (<> `(html5:a :class "btn btn-secondary btn-sm"
			              :href ,(concatenate
			                      'string
			                      "/ecm/edit?diary-entry="
			                      (princ-to-string _id)
			                      "&go-back-to-claim="(princ-to-string claim-id)
                                  "&access[type]=claim&access[id]="
			                      (princ-to-string claim-id)))
	          (<> "Edit"))
	        (<> `(html5:a :class "btn btn-secondary btn-sm"
			              :href ,(concatenate
			                      'string
			                      "/ecm/delete?diary-entry="
			                      (princ-to-string _id)
			                      "&go-back-to-claim= "(princ-to-string claim-id)
                                  "&access[type]=claim&access[id]="
			                      (princ-to-string claim-id)))
	          (<> "Delete")))



	      ))
      (<> '(html5:div :class "clearfix"))
      (<> '(html5:pre :class "blockquote")
	    (<> :text (getjso "note" diary-entry))))))






(defparameter *col-class* "col")

(defun <claim> (claim &key active-tab)
  (let* ((cause (getjso "cause" claim))
	     (risk (getjso "risk" claim))
                                        ;(loss (getjso "loss" claim))
                                        ;(claim-number (getjso "_id" claim))
         (industry (getjso "industry" claim))

	     (status-detail
	       (getjso "status_detail" claim))
	     (outstanding-diary
	       (getjso "outstanding_diary" claim))
	     (user (ecm/user:user))
	     (user-id (ecm/user:user-id user))
	     (user-outstanding-diary
	       (remove-if-not
	        (lambda (de)
	          (equalp (ecm/json:getjso* "user._id" de)
		              user-id))
	        outstanding-diary))
	     (firm-e (getjso "restoration_firm_emergency" claim))
         (firm-r (getjso "restoration_firm_repair" claim))
         (defense-counsel (getjso "defense_counsel" claim))
         (coverage-counsel (getjso "coverage_counsel" claim))
	     (claimant (getjso "claimant" claim)))
    (<style-claim>)
    (<> '(html5:div
	      :class "container-fluid")

      ;; Top of Page
      (<> '(html5:div :class "row")
	    (when user-outstanding-diary
	      (<> '(html5:div :class  "col-10 offset-md-1")
	        (dolist (d user-outstanding-diary)
	          (<diary> d))
	        (<> (hr)))))


      (<> '(html5:div :class "row")


	    (let ((col-class "col border-top pt-3"))
	      (macrolet ((col ((&key (when t))
			               &body body)
		               `(,@(if (eq t when) '(progn) `(when ,when))
			             (<> (html5:div :class col-class)
			               ,@body))))


	        ;; ** Risk / Cause / examine
	        (col (:when risk)
		         (<risk> claim)
                 (when industry (<industry> claim))
		         (when cause
		           (<cause> cause))
		         (when claimant
		           (<claimant> claimant))
                 (when firm-e
                   (<restoration-firm> firm-e))
                 (when firm-r
                   (<restoration-firm> firm-r "Restoration Firm (repair)"))
                 (when coverage-counsel
                   (<coverage-counsel> coverage-counsel))
                 (when defense-counsel
                   (<defense-counsel> defense-counsel))
		         (<examine> claim))

            (<> '(html5:div :class "d-md-none w-100"))
	        ;; ** Loss / Status
	        (col ()
		         (<loss> claim)
		         (when status-detail
		           (<status-detail> status-detail)))




	        ))
		)
      (<> '(html5:hr)))
    (<script-claim>)))




(defun <claim-heading> (claim-number insured status claim)
  (let ((outstanding-diary
	        (getjso "outstanding_diary" claim)))

    (multiple-value-bind (claim insured status)
	      (values-claim/insured/status claim-number insured status
				                             :style "")
      (funcall claim)
      (<> :unescaped "&nbsp;")
      (funcall status)
      (<> :unescaped "&nbsp; ")
      (funcall insured))
    (when outstanding-diary
      (<> (button
	         :type "button"
	         :style "padding: 0.25rem; margin-left: 1em; margin-top:0.5rem;"
	         :class "alert alert-danger ecm-window-minimize btn btn-outline-danger"
	         :data-toggle "collapse"
	         :data-target "#claimInfo"
	         :aria-expanded "true"
	         :aria-controls "claimInfo"
	         :data-claim-ui "info")
	        (<> :text (if (> (length outstanding-diary) 1)
		                    (cat (length outstanding-diary) " ")
		                    "")
	            "Outstanding Diar"
	            (if (> (length outstanding-diary) 1)
		              "ies"
		              "y"))))))



(defun corpus-name (corpus)
  (let* ((first-name (getjso "first_name" corpus))
	     (last-name (getjso "last_name" corpus))
	     (company-name (getjso "company_name" corpus))
	     (province (getjso "province" corpus))
	     (short-name (and province (getjso "short_name" province))))
    (with-output-to-string (s)
      (when first-name (princ first-name s))
      (when last-name
	    (princ #\Space s)
	    (princ last-name s))
      (when (and (or first-name last-name)
		         company-name)
	    (princ ", " s))
      (when company-name (princ company-name s))
      (when (and (or first-name last-name company-name)
		         short-name)
	    (princ ", " s))
      (when short-name (princ short-name s)))))

(defun values-claim/insured/status (claim-number insured status
				                    &key (style  "color : white"))
  (flet ((claim ()
	       (<> '(html5:h3 :style "display: inline-block")
	         (<> '(html5:small :class "text-muted")
	           (<> :unescaped "Claim&nbsp;#"))
	         (<> :text claim-number)))
	     (insured ()
           (if (eql :null insured)
               (<> :text "NO INSURED")
	           (<> (h3 :style "display: inline-block")
	             (<> '(html5:small :class "text-muted") "Insured")
	             (<> :unescaped "&nbsp;")
	             (<> (html5:a :href (concatenate
				                     'string "/ecm/view?person="
				                     (princ-to-string (getjso "_id" insured)))
			                  :style style
			                  :target "_blank"
			                  :title (corpus-name insured))
		           (<> :text (corpus-name insured))))))
	     (status ()
	       (<> '(html5:h3 :style "display: inline-block")
	         (<> '(html5:small :class "text-muted")
		       (<> :unescaped "&nbsp;" "status" "&nbsp;"))
	         (<> :text status))))
    (Values #'claim #'insured #'status)))



(defun <focus-on-timecard> (timecard-id)
  (when timecard-id
    (<> 'html5:script
      (concatenate
       'string "
$(window).on('load', function () {

$('#viewTabs').contents().find('#Timecards input[name=\"timecard-id\"][value="
       (princ-to-string timecard-id)"]').parent().parent().find('td').css('font-size' , '150%');
  $('html, body').animate({
    scrollTop: (
       $('#viewTabs').contents().find('#Timecards input[name=\"timecard-id\"][value=" (princ-to-string timecard-id)
       "]').parent().offset().top + $('#viewTabs').offset().top - 100
  )
   },250);

});"))))



(defvar *user-claim-ui*)

(defun <claim-script-collapse-ui> (claim-id)
  (<> (script)
    (ps:ps*
     `(defun claim-ui-show (claim-id field show)
	    (let ((data ({})))
	      (setf (ps:aref data field) show)
                                        ; (console.log field show)
	      ($.ajax
	       ({}
	         :type "POST"
	         :url (+ "/ecm/ui/claim/" claim-id)
	         :content-type "application/json; charset=utf-8"
	         :data-type "json"
	         :data (|JSON.stringify| data)))))
     `($ (lambda ()
	       (|.|($ ".collapse[data-claim-ui]")
	           (on "hide.bs.collapse"
		           (lambda ()
		             (let ((field (|.| ($ this ) (data "claim-ui"))))
		               (claim-ui-show ,claim-id field ps:false))
		             )))
	       (|.|($ ".collapse[data-claim-ui]")
	           (on "show.bs.collapse"
		           (lambda ()
		             (let ((field (|.| ($ this ) (data "claim-ui"))))
		               (claim-ui-show ,claim-id field t)))))

	       ;; when the tab is clicked on and the tab card is
	       ;; collapsed, show it and change the icon
	       (|.| ($ "[data-toggle=\"tab\"]")
		        (on "shown.bs.tab"
		            (lambda ()
		              (|.| ($ "div[data-claim-ui=\"tabs\"]")
			               (collapse "show"))
		              (|.| ($ "a[data-claim-ui=\"tabs\"]")
			               (find "i")
			               (|removeClass| "fa-caret-square-o-up")
			               (|removeClass| "fa-caret-square-o-down")
			               (add-class "fa-caret-square-o-down"))
		              ))))))))

(defun group-members (membs)
  (loop :for mem :in membs :do (<> (html5:a :class "dropdown-item" :href (format nil "/ecm/claim/~A" (getjso "claim_id" mem)))
                                 (<> :text (format nil "~A ~A%" (getjso "claim_id" mem)
                                                   (or (getjso "percent" mem) 100))))))


(defun <claim-page> (claim-id
                     &key
				               (active-tab "Transactions" active-tab-provided?)
				               timecard-id)
  (let* ((claim-id (ignore-errors (parse-integer claim-id)))
	       (claim-crux-json
	         (postmodern:query
	          (:select (:jsi.claim-crux claim-id))
	          :single))
	       (claim
	         (progn (unless claim-crux-json
		                (error "No CLaim Crux for ~A" claim-id))

		              (ecm/json:read-json-from-string
		               claim-crux-json)))
	       (status (getjso "status" claim))
                                        ; (cause (getjso "cause" claim))
	       (risk (getjso "risk" claim))
                                        ;  (loss (getjso "loss" claim))
	       (policy (getjso "policy" risk))
	       (insured (getjso "insured" policy))
	       (claim-number (getjso "_id" claim))
         (group (getjso "group" claim))
         (leader (getjso "leader" group))
         (peers (getjso "peers" group))
         (followers (getjso "followers" group))
	       (*user-claim-ui* (ecm/entity/ui:user-claim-ui claim-id))
	       (ui-info (ecm/json:from-json-bool (getjso "info" *user-claim-ui*)))
	       (ui-balance (ecm/json:from-json-bool (getjso "balance" *user-claim-ui*)))
	       (ui-tabs (ecm/json:from-json-bool (getjso "tabs" *user-claim-ui*)))
	       (active-tab (if (and active-tab-provided? active-tab)
			                   active-tab
			                   (or (getjso "active_tab" *user-claim-ui*)
			                       "Timecards"))))
    (<> (div :class "modal fade" :id "ecmModal" :tabindex "-1" :role "dialog" :aria-hidden "true")
      (<> (div :class "modal-dialog modal-lg"
	             :style "width:90%;height:90%;max-width:none;z-index:10000")
	      (<> (div :class "modal-content"
		             :style "height:100%;max-height:none;")
	        (<> (div :class "modal-header text-center")

	          (<> (h4 :class "modal-title mx-auto"))
            (<> (button :type "button" :class "close" :data-dismiss "modal" :aria-label "Close"
                        :style "margin:inherit")
	            (<> (span :aria-hidden "true")
		            (<> :unescaped "&times;"))))
	        (<> (div :class "modal-body"
		               :style "height:90%;;")
	          (<> "...")))))

    (<claim-navbar> claim-number insured status)
    (<> (script)
      (ps:ps* `($ (lambda()
		                (let ((sel (+ "[data-ecm-tab=\"" ,active-tab "\"]")))
		                  (|.| ($ sel) (tab "show"))
		                  )))))

    (ecm/ui/claim/clone:<clone-claim-modal> claim-number)

    ;; * Collapsable

    (<> (div :id "claimAccordion" :role "tablist" :aria-multiselectable "true")
      ;; ** Info
      (<> (div :class "card")
        ;; *** Header
        (<> (div :class "card-header clearfix" :role "tab" :id "ClaimHeading"
	               :style "text-align : center")
	        (<> (div :class "hover-btn")
	          (<> (a :class "close ecm-window-minimize"
		               :data-toggle "collapse"
		               :href "#claimInfo"
		               :aria-expanded "true"
 		               :aria-controls "claimInfo"
		               :data-claim-ui "info")
	            (<> (html5:i
		               :class (cat "fa "
			                         (if ui-info
				                           "fa-caret-square-o-up"
				                           "fa-caret-square-o-down"))
		               :aria-hidden "true")
	              " ")))
 (when (ecm/user:user-can-edit-p)
      (<> :style '|.dbtn:hover b{
        border: 1px solid gray;
        border-radius: 2px;
        background-color: lightgray;
        cursor:pointer;
        color: black;
            }
  .dbtn { color: gray }
   .dbtn button i  {
        color: unset;
     }
  .dbtn button {
        color: unset;
     }
dropdown-item:focus, .dropdown-item:hover {
	color: #0056b3;
	text-decoration: underline;
	background-color: transparent;
}
|)

 ;;    (<> :unescaped '|<div class="dropdown dbtn" style="float:left;">
;;   <button class="btn" type="button" id="dropdownMenuButton"
;;          data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
;;     <i class="fa fa-bars"> </i>
;;   </button>
;;   <div class="dropdown-menu" aria-labelledby="dropdownMenuButton">
;;    <a class="dropdown-item" href="|)

;;       (<> :text (format nil "/ecm/claim/~A/edit" claim-number))
;;       (<> :unescaped '|">Edit</a>
;;   </div>
;; </div>
;; |)

             (<> :unescaped '|<div class="dropdown dbtn dropright" style="float:left;">
  <button class="btn" type="button" id="dropdownMenuButton"
         data-toggle="dropdown" aria-haspopup="true" aria-expanded="false"
         title="Group Information" data-offet="40">
<i class="fa | (if (or leader followers peers)
                   "fa-object-group" "fa-bars") '|"></i>
  </button>
  <div class=")
dropdown-menu" aria-labelledby="dropdownMenuButton">
   <a class="dropdown-item" href="#"
        onclick="cloneClaim.show()">Clone with #| claim-id '| as master</a>
  |)
             (when leader
               (<> '(h5 :class "dropdown-header") "Master")
               (<> (html5:a :href (format nil "/ecm/claim/~A" leader) :class "dropdown-item")
                 (<> :text "#" leader)))

             (when peers (<> '(h5 :class "dropdown-header") "Peers")
                   (group-members peers))
              (when followers
               (<> '(h5 :class "dropdown-header") "Followers")
               (group-members followers))
             (<> :unescaped '|
  </div>
</div>





|)

          (<claim-heading> claim-number insured status claim)
  (when leader (<> 'h5 "Master: " (<> (html5:a :href (format nil "/ecm/claim/~A" leader))
                                              (<> :text "#" leader))))
         ))
        ;; *** Body
        (<> (div :id "claimInfo"
	               :class (cat "collapse"
			                       (when ui-info " show"))
	               :data-claim-ui "info"
	               :role "tabpanel"
	               :aria-labelledby="claimHeading")
	        (<> (div :class "card-block")
	          (<claim> claim :active-tab active-tab))))

      ;; ** Balance
      (<claim-balance-card> claim :ui-balance ui-balance)
      ;; ** Tabs
      (<> (div :class "card persist-area"
	             :data-persistent-class "floatingHeader"
	             :data-persistent-header ".persist-header")
        (ecm/ui/tabs:<claim-tabs>
         claim
         :active nil
         :ui-tabs ui-tabs))))
  (<focus-on-timecard> timecard-id)
  (<> (div  :style "display:none;")
    (<> (iframe :src "/ecm/timezone"
		            :frameborder 0
		            :height "30px")))
  (<claim-script-collapse-ui> claim-id)

  (<> (style)
    ".floatingHeader {
  position: fixed;
  top: 10px;
  opacity: 0.9;
  z-index:5000;
  border: 2px solid lightgrey;
  border-radius: 0.5em;
  background-color: #efefef;
}

.floatingHeader .ecm-window-minimize {
    color:white;
    opacity:1
}
"
    ".floatingTr {
  position: fixed;
  top: 55px;
}
")
  (ecm/ui/persistent-header:<persistent-header-script>))


(defun claim/get (claim-id)
  (unless claim-id
    (error "No CLaim ID given"))
  (let ((title (concatenate 'string
			                claim-id " :: Claim on ECM"))
	    (timecard (ecm/hunchentoot:parameter-or-nil
		           "timecard"
		           :identity #'parse-integer))
	    (active-tab (or (ecm/hunchentoot:parameter-or-nil
			             "active-tab")
			            nil)))

    (<> (ecm/ui/page:page
	     :title title
         :ecm-body-class "container-fluid"
	     :datatables t)
      (let* ((read?
	           (postmodern:query
	            (:select (:app_user_can_read 'claim)
		         :FROM 'claim
		         :where (:= 'claim-id (:type claim-id integer)))
	            :single)))
	    (if (eq read? t)
	        (<claim-page>
	         claim-id
	         :timecard-id timecard :active-tab active-tab)
	        (<> :text "No access to this claim"))))))
