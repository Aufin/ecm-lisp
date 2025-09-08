(uiop:define-package :ecm/ui/claim/balance
    (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.| #:$ #:$. )
  (:import-from :ecm/api/find)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/ui/risk
		#:<risk>)
  (:import-from :ecm/ui/tabs)
  (:import-from :ecm/ui/iframe)
  (:import-from :ecm/ui/persistent-header)
  (:import-from :ecm/local-time)
  (:import-from :ecm/entity/ui)
  (:import-from :ecm/endpoint/attachment)
  (:import-from :ecm/ui/utility #:cat)
  (:import-from :ecm/user
		#:user #:user-id)
  (:import-from :ecm/json
		#:getjso)
  (:export #:<claim-balance-card>))
(in-package :ecm/ui/claim/balance)


(defun <claim-balance-script> (claim-id)
  (<> (script)    
    (ps:ps* 
     `($ (lambda ()
	   
	   (defun refresh-claim-balance-card (&key (id "#claimBalanceCard"))
	     (let ((body ($ id)))
	       ($. body (empty)
		   (append "<div id=\"IFrameLoading\" style=\"font-size:500%;width:100%\" class=\"text-xs-center\"><i  class=\"fa fa-spinner fa-spin spin-normal\"></i></div>"))
	       ($.get (+ "/ecm/claim/" ,claim-id "/card/balance")
		      (lambda (data)
			($. body (replace-with data))))))
	       
	   (setf claim-balance ({} "refresh" refresh-claim-balance-card)))))))
  
(defun <claim-balance-heading> (claim)
  (let* ((balance (getjso "balance" claim))
	 (deductible (getjso "deductible" balance))
	 (incurred (getjso "incurred" balance))
	 (authority(getjso "authority" claim))
	 (over-authority (getjso "over_authority" authority))
	 (claim-authority (getjso "claim_authority" authority))
	 (contract-authority (getjso "contract_authority" authority))
	 (over-by (when (and over-authority incurred
			     (or claim-authority contract-authority))
		    (- (parse-number:parse-number incurred)
		       (parse-number:parse-number (or claim-authority contract-authority))))))
    
    (<> (h3 :style "display:inline-block;margin-right:0.5em; white-space:nowrap")
      (<> (span :class "text-muted")
	(<> :unescaped "Incurred&nbsp;$"))
      (<> :text incurred))

    (<> (h3 :style "display:inline-block;margin-right:0.5em; white-space:nowrap")
      (<> (span :class "text-muted")
	(<> :unescaped "Deductible: $"))
      (<> :text deductible))

    (when (eq :true over-authority)
      (<> (button
	   :type "button"
	   :style "padding: 0.25rem; margin-left: 1em; margin-top:0.5rem;"
	   :class "alert alert-danger ecm-window-minimize btn btn-outline-danger"
	   :data-toggle "collapse"
	   :data-target "#claimBalance")
	(<> :text "Over Authority by $" (format nil "~$" over-by))))))

(defun <deductible> (balance)
  (let ((deductible (getjso "deductible" balance))
	(recovered-deductible (getjso "recovered_deductible" balance))
	(outstanding-deductible (getjso "outstanding_deductible" balance)))
    (<> (div :class "row")
      (<> (div :class "col-md-12 text-xs-center")
	(<> 'h3	  (<> :text "Deductible $"
		      deductible))))
    (<> '(html5:div :class "row")
      (<> '(html5:div :class "col-sm-3")
	(<> '(html5:h5 :class "text-muted")
	  (<> "Outstanding")))
      (<> '(html5:div :class "col-sm-3 h5")
	(<> :text "$" outstanding-deductible))
      (<> '(html5:div :class "col-sm-3")
	(<> '(html5:h5 :class "text-muted")
	  (<> "Recovered")))
      (<> '(html5:div :class "col-sm-3")
	(<> :text "$" recovered-deductible)))))

(defun <balance> (balance)
  (let (                                ;(incurred (getjso "incurred" balance))
	      (outstanding-reserve (getjso "outstanding_reserve" balance))
        (limit (getjso "limit_of_cover" balance))
	      (total-paid (getjso "total_paid" balance))
	      (headings-balance (ecm/json:null->nil
			                     (getjso "headings_balance" balance))))
    (<> '(html5:table :class "table")
	    (<> '(html5:tbody)
	      (<> 'html5:tr
	        (<> 'html5:th)
	        (<> 'html5:th (<> "Limit of Cover"))
	        (<> 'html5:th (<> "Outstanding Reserve"))
	        (<> 'html5:th (<> "Total Paid")))
	      (when headings-balance
	        (dolist (h headings-balance)
	          (<> 'html5:tr
		          (<> 'html5:th (<> :unescaped (getjso "heading" h)))
              (<> 'html5:td
                (let ((l (getjso "limit_of_cover" h)))
                  (unless (eq :null l)  (<> :text "$" l))))
              (<> 'html5:td
		            (<> :text "$" (getjso "outstanding_reserve" h)))
		          (<> 'html5:td
		            (<> :text "$" (getjso "total_paid" h))))))
	      (<> '(html5:tr)
	        (<> '(html5:td)
	          (<> "Total"))
	        (<> '(html5:td :style "border-top:1px solid black")
            (unless (eq :null limit) 
              (<> :text "$" limit)))
	        (<> '(html5:td :style "border-top:1px solid black") (<> :text "$" outstanding-reserve))
	        (<> '(html5:td :style "border-top:1px solid black")
	          (<> :text "$" total-paid)))))))

(defun <authority> (authority balance)
  (let* ((over-authority (getjso "over_authority" authority))
	 (claim-authority (getjso "claim_authority" authority))
	 (contract-authority (getjso "contract_authority" authority))
	 (incurred (getjso "incurred" balance))
	 (over-by (when (and over-authority incurred
			     (or claim-authority contract-authority))
		    (- (parse-number:parse-number incurred)
		       (parse-number:parse-number (or claim-authority contract-authority)))))
	 (authority (when (and
			   incurred
			   (or claim-authority contract-authority))
		      (- 
		       (parse-number:parse-number
			(or claim-authority contract-authority))
		       (parse-number:parse-number incurred)))))
    (<> (div :class "row")
      (<> (div :class "col-md-12 text-xs-center")
	(if (not (or claim-authority contract-authority))
	    (<> (h3 :class "align-middle") "No Authority Amount")
	    (<> (h3)
	      (if (eq :true over-authority)
		  (<> (span :class "alert alert-danger"
			    :style "padding: 0.25rem;")
		    (<> :text "Over Authority by $" (format nil "~$" over-by)))
		  (<> :text "Authority $" authority))))))
    (when (or claim-authority contract-authority)
      (<> '(html5:div :class "row")
	(<> '(html5:div :class "col-sm-3")
	  (<> '(html5:h5 :class "text-muted")
	    (<> "Claim")))
	(<> '(html5:div :class "col-sm-3")
	  (<> :text "$" claim-authority))
	(<> '(html5:div :class "col-sm-3")
	  (<> '(html5:h5 :class "text-muted")
	    (<> "Contract")))
	(<> '(html5:div :class "col-sm-3")
	  (<> :text "$" contract-authority))))))


(defun <claim-balance> (claim)
  (<> '(html5:div
	:class "container-fluid collapse-hover")
    (<> (div :class "row")
      (<> (html5:div :class "col-md-10 offset-md-1 col-sm-12")
	(<balance> (getjso "balance" claim))))
        (<> '(html5:div :class "row")
      (let ((col-class "col-md-6"))
	(macrolet ((col ((&key (when t) (style nil))
			 &body body)
		     `(,@(if (eq t when) '(progn) `(when ,when))
			 (<> (html5:div :class col-class
					,@(when style (list :style style)))
			   ,@body))))
	  (col () (<> (div :style "position: absolute; right: 0%; top: 25% ; border-right: 1px solid rgba(0,0,0,.1);")
		    (<> :unescaped "&nbsp;"))
	       (<deductible> (getjso "balance" claim)))
	  (col () (<authority> (getjso "authority" claim)
			       (getjso "balance" claim))))))
    (<> '(html5:hr))))

(defun <claim-balance-card>
    (claim
     &key (ui-balance
	   (ecm/json:from-json-bool
	    (getjso "balance"
		    (ecm/entity/ui:user-claim-ui
		     (getjso "_id" claim))))))
  (<> (div :class "card"
	   :id "claimBalanceCard")
    ;; *** Header
    (<> (div :class "card-header clearfix"
	     :role "tab" :id "ClaimBalanceHeading"
	     :style "text-align : center")
      (<> (div :class "hover-btn")
	(<> (a :class "close ecm-window-minimize"
	       :data-toggle "collapse"
	       :href "#claimBalance"
	       :aria-expanded "true"
	       :aria-controls"claimBalance")
	  (<> (html5:i :class (cat "fa "
				   (if ui-balance
				       "fa-caret-square-o-up"
				       "fa-caret-square-o-down"))
		       :aria-hidden "true")
	    " ")))
      (<claim-balance-heading> claim))
    ;; *** Body
    (<> (div :id "claimBalance"
	     :data-claim-ui "balance"
	     :class (cat "collapse"
			 (when ui-balance " show"))
	     :role "tabpanel"
	     :aria-labelledby="claimBalanceHeading")
      (<> (div :class "card-block")
	(<claim-balance> claim)))
    (<claim-balance-script> (getjso "_id" claim))))
