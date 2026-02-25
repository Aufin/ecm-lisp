(defpackage :ecm/ui/transaction
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.| #:$.)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/ui/utility
		#:cat)
  (:import-from :ecm/entity/corpus)
  (:import-from :ecm/ui/corpus)
  (:import-from
   :ecm/entity/transaction)
  
  (:import-from :ecm/json #:getjso)
  (:export #:<edit-transaction-modal>
	   #:<claim-transaction-tr>
	   #:<claim-transaction-table>
	   #:edit-transaction-page
	   #:create-transaction-page
	   #:edit-transaction-inline-finished))
(in-package :ecm/ui/transaction)

(defun find-possible-interims (claim-id)
  (postmodern:query
   "SELECT to_json(ti) FROM timecard_interim ti WHERE claim_id = $1 AND invoice_id IS NULL ORDER BY date" claim-id :column))


(defun <transaction-interim-select> (claim-id &key (selected nil))
  (<> (html5:select :name "interim-id"
					:class "form-control")
    (<> (html5:option :value "") (<> :text ""))
    (dolist (ti (find-possible-interims claim-id))
	  (let* ((tij (st-json:read-json-from-string ti))
			 (text (st-json:getjso "date" tij))
			 (id (st-json:getjso "timecard_interim_id" tij)))
		
		(<> `(html5:option :value ,id ,@(when (equalp id selected)
										 '(:selected t)))
		  (<> :text (split-sequence:split-sequence #\T text)))))))



(defun <transaction-style> (&key (id "ecmClaimTransactionTable"))
  (<> (html5:style)
    ".cheque {
  position: relative ;
 
 /*  color:blue;
  background-color:green; */
 background-color:white; 
  box-shadow: 0 2px 4px 0 rgba(0,0,0,0.16),0 2px 10px 0 rgba(0,0,0,0.12)!important;
  border: 1px solid lightgray;
  border-radius: 0.5rem;
  padding: 0.5rem;
  z-index: 100;

}


/* Responsive datatable */

.dtr-details li  {
  padding:0px;
  display:inline;
 border:0px;
}

#ecmClaimTransactionTable ul.dtr-details {
  width:100%;
}

#ecmClaimTransactionTable ul.dtr-details .transaction-buttons {
   display: inline-block;
   position:absolute;
   top: -1rem;
   left: 35vw;
  }
#ecmClaimTransactionTable .dtr-title {
  display:none;
}

#ecmClaimTransactionTable li {
  border:0px;
  display:inline;
}

.dtr-details .transaction-buttons {

  display: inline;
  margin: auto;
}
.dtr-details .transaction-buttons .btn-group {

  display: inline;
  margin: auto;
}

.ui-datepicker
    {
        z-index: 1003 !important; /* must be > than popup editor (1002) */
    }

 .zold-edit {
    position: absolute ;
    top: -2em;
}
 "
    "
     #"id"  { 
 z-index : -1;
 width:100%;
 max-width: 100vw;

}

.div-to-display {
    position: absolute;
    top: 7.5%;
    left: 25%;
    padding:10px;
    margin:0px;
    color:black;
    background-color:white;
    border: 1px solid lightgray;
    border-radius: 0.5rem;
    z-index: 10;
    -ms-filter: \"progid:DXImageTransform.Microsoft.Alpha(Opacity=70)\";       /* IE 8 */
    filter: alpha(opacity=80);  /* IE 5-7 */
    -moz-opacity: 0.7;          /* Netscape */
    -khtml-opacity: 0.7;        /* Safari 1.x */
    opacity: 0.7;               /* Good browsers */
}

td:hover > .div-to-display {
    display: block
}

#ecmClaimTransactionTable_wrapper .row {
  margin:auto;
  width:100%;} 

.modal-body {
	overflow: auto;
	max-height: 90%;
  z-index: 10043;

	background:
		/* Shadow covers */

		linear-gradient(white 30%, rgba(255,255,255,0)),
		linear-gradient(rgba(255,255,255,0), white 70%) 0 100%,
		
		/* Shadows */


		radial-gradient(50% 0, farthest-side, rgba(0,0,0,.2), rgba(0,0,0,0)),
		radial-gradient(50% 100%,farthest-side, rgba(0,0,0,.2), rgba(0,0,0,0)) 0 100%;
	background:
		/* Shadow covers */
		linear-gradient(white 30%, rgba(255,255,255,0)),
		linear-gradient(rgba(255,255,255,0), white 70%) 0 100%,
		
		/* Shadows */
		radial-gradient(farthest-side at 50% 0, rgba(0,0,0,.2), rgba(0,0,0,0)),
		radial-gradient(farthest-side at 50% 100%, rgba(0,0,0,.2), rgba(0,0,0,0)) 0 100%;
	background-repeat: no-repeat;
	background-color: white;
	background-size: 100% 40px, 100% 40px, 100% 14px, 100% 14px;
	
	/* Opera doesn't support this in the shorthand */
	background-attachment: local, local, scroll, scroll;
}



"))

(defun <transaction-ui-script> ()
  (<> (html5:script) '|

function trannyUserInterface() {
    var ttype_select = $('select[name="transaction-type"]');
    var ttype = $(ttype_select).val();
    var c1 = $('.cheque');
    var c2 = $('#cheque');
    var or1 = $('#open_reserve');

    const // interim_select = document.querySelector('select[name=interim-id]'),
          heading_select = document.querySelector('select[name=transaction-heading]'),
          htype = heading_select.value
          //isp = interim_select.parentElement.parentElement,
          ishide = !(htype == 'TPA' && ttype == 'Cheque - Expense')
   // isp.children[2].hidden = ishide
   // isp.children[3].hidden = ishide

  //  console.debug('isel', interim_select, isp);

    $(c1).hide(); $(c2).hide(); $(or1).hide();

    if (ttype == 'Open Reserve') {
        $(or1).show()
    } else if (ttype != "Reserve Adjustment") {
        $(c1).show(); $(c2).show();
    }

}
$(function() {
    trannyUserInterface();
    $('select[name="transaction-type"]').change(function() {
            trannyUserInterface();
    })
    const heading_select = document.querySelector('select[name=transaction-heading]')
   $(heading_select).change(function() {
            trannyUserInterface();
    })

});
|)
  )

(defun create-or-update-transaction-elements (claim-id
											  &optional
												(error nil)
												(date nil)
												(type nil)
												(expense-type nil)
												(heading nil)
												(amount nil)
												(limit-of-cover nil)
												(interim-id nil)
												(payee nil)
												(recipient nil)
												(cheque-number nil)
												(reference-number nil)
												(schemes-advance-number nil)
												(date-paid nil))

											 
  (when error
	(<> (p :class "text-danger")
	  (<> (pre) (<> :text (princ-to-string error)))))
  (<> (style)
	".form-control { margin-bottom : 2px; }:")
  (<> (form :method "POST")
    (macrolet ((row (name value name2 value2 &key (head 'h3))
		         `(<> (div :class "row")
					(<> (div :class "col-md-2") (<> (,head) (<> (:text ,name))))
		            (<> (div :class "col-md-4") ,value)
		            (<> (div :class "col-md-2") (<> (,head) (<> (:text ,name2))))
		            (<> (div :class "col-md-4") ,value2))))
      (<transaction-style>)
      (row "Date:" (<transaction-datepicker>
		            :date date)
	       "Type:" (<transaction-types-select>
		            :selected type))
      (row "Heading:" (<transaction-headings-select>
		               :selected heading)
	       "Amount:" (<transaction-amount> amount))
      (<> (div :id "cheque")
        (row "Expense:" (<transaction-expense-types-select>
		                 :selected expense-type)
			 "" ""
	         #+(or)"Interim:"
			 #+(or)(<transaction-interim-select>
					claim-id :selected interim-id)
			 )

		)
      (<> (div :id "open_reserve" :style "padding-top:10px;")
        (<> (div :class "row justify-content-center")
		  (<> (div :class "col-md-2")
		    (<> (h3) "Limit Of Cover"))
		  (<> (div :class "col-md-4")
		    (<transaction-limit-of-cover> limit-of-cover))))
      (<> (div :class "cheque")
	    (<> (div :class "row" :style "margin-bottom:5px")
	      (<> (div :class "col-md-2")
	        (<> 'h3 (<> (:text "Payee"))))
	      (<> (div :class "col-md-10")
	        (ecm/ui/corpus:<corpus-input> payee)))
		
	    (<> (div :class "row" :style "margin-bottom:5px")
	      (<> (div :class "col-md-3")
	        (<> 'h3 (<> (:text "Cheque Number"))))
	      (<> (div :class "col-md-9")
	        (<> (input :type "text" :class "form-control" 
		               :name "transaction-cheque-number"
		               :value cheque-number))))
		(<> (div :class "row" :style "margin-bottom:5px")
	      (<> (div :class "col-md-2")
	        (<> 'h3 (<> (:text "Date Paid"))))
	      (<> (div :class "col-md-10")
	        (<transaction-datepicker>
			 :date (or date-paid "")
			 :name "transaction-date-paid")))
	    (row "Schemes Advance #" (<> (input :type "text" :class "form-control" 
				                            :name "transaction-schemes-advance-number"
				                            :value schemes-advance-number))
	         "Reference #" (<> (input :type "text" :class "form-control" 
				                      :name "transaction-reference-number"
				                      :value reference-number))
	         :head h5)
        (<> (div :class "row" :style "margin-bottom:5px")
	      (<> (div :class "col-md-4")
	        (<> 'h3 (<> (:text "Recipient")))
            (<> 'small "(cheque to be sent to)"))
	      (<> (div :class "col-md-8")
	        (ecm/ui/corpus:<corpus-input> recipient :name "recipient-id" :prefix "r-")))
        )

      (<> (button :type "submit" :class "btn btn-success risk-edit-submit" :style "float:left")
		"Create Transaction")
	  (<> (a :id "cancelUpdateTransaction"
		     :class "btn btn-danger" :style "float:right")
		"Cancel"))
    (<> (script) '|$( "#cancelUpdateTransaction" ).click(function( event ) {
  event.preventDefault();
  parent.risk.hide_edit();
});|)
    (<transaction-ui-script>)

    ))
(defun create-transaction-page
    (claim-id
	 &key
	   (inline t)
	   (error nil)
	   (date)
	   (date-paid)
	   (type)
	   (expense-type)
	   (heading)
	   (amount)
       (limit-of-cover)
	   (interim-id)
	   (payee)
	   (recipient)
	   (cheque-number)
	   (reference-number)
	   (schemes-advance-number))
  (declare (ignorable claim-id))
  (<> (ecm/ui/page:page :title (unless inline "transaction"))
	(create-or-update-transaction-elements
	 claim-id error date type expense-type heading amount limit-of-cover interim-id payee recipient cheque-number reference-number schemes-advance-number date-paid)))
  
(defun edit-transaction-page
    (transaction
	 &key
	   (claim-id (getjso "claim_id" transaction))
	   (inline t)
	   (error nil)
	   (date (getjso "transaction_date" transaction))
	   (date-paid (getjso "date_paid" transaction))
	   (type (getjso  "type" transaction))
	   (expense-type (getjso "expense_type"
						     transaction))
	   (heading (getjso "heading" transaction))
	   (amount (getjso "amount" transaction))
	   (approved (ecm/json:from-json-bool
				  (getjso "appoved" transaction)))
	   (limit-of-cover (getjso "limit_of_cover" transaction))
	   (interim-id (ecm/json:getjso "interim_id" transaction))
	   (payee (ecm/json:getjso* "cheque.payee" transaction))
	   (recipient (ecm/json:getjso* "cheque.recipient" transaction))
	   (cheque-number (ecm/json:getjso* "cheque.cheque_number" transaction))
	   (reference-number (ecm/json:getjso* "cheque.reference_number" transaction))
	   (schemes-advance-number (ecm/json:getjso* "cheque.schemes_advance_number" transaction)))
                                        ;  (break "~A" approved)
  (<> (ecm/ui/page:page :title (unless inline "transaction"))
	(when (not inline) (<> (ecm/ui/navbar:navbar)))
	(<> :text (format nil "Inline ~W" (not inline)))
	(create-or-update-transaction-elements
	 claim-id error date type expense-type heading amount limit-of-cover interim-id payee recipient cheque-number reference-number schemes-advance-number date-paid)
    ))

(defun edit-transaction-inline-finished (transaction-id claim-id)
  (<> (ecm/ui/page:page)
    (<> (script)
      (ps:ps* `($ (lambda ()
		    (let* ((table ($ "#ecmClaimTransactionTable"
				     window.parent.document))
			   (parent ($. table (parent)))
			   (tr ($ ,(cat "[data-transaction-id=" transaction-id "]")
				  window.parent.document)))
		      (let* ((trans window.parent.transaction)
			     (show-edit trans.show_edit)
			     (create trans.create)
			     (scroll trans.scroll))

			(trans.hide_edit)			
			(window.parent.claim-balance.refresh)

            (|.| window.parent.transaction.table
			     (*data-table) (destroy))
			(let ((p ($. window.parent.transaction.table (parent))))
			  ($. window.parent.transaction.table (remove))
			  ($. p (empty)
			      (append "<div id=\"IFrameLoading\" style=\"font-size:1000%;width:100%\" class=\"text-xs-center\"><i  class=\"fa fa-spinner fa-spin spin-normal\"></i></div>"))
			  ($.get (+ "/ecm/claim/" ,claim-id "/transaction-table")
				 (lambda (data)
				   ($. p (empty))
				   ($. p (append data))
				   (window.parent.transaction-Datatable)
				   (scroll ,transaction-id)
				 ;; (console.log ($ ".tranny-edit" window.parent.document))
				 (|.| ($ ".tranny-edit" window.parent.document)
				      (click (lambda( event )
					       (event.prevent-Default)
					       (console.log show-edit)
						  (show-edit
						   ($. this (data "transaction-id"))
						   ($. this (data"claim-id"))))))
				   (|.| ($ "#tranCreateNew" window.parent.document)
					(click (lambda( event )
						 (event.prevent-Default)
						 (console.log show-edit)
						 (create ,claim-id)))))))))))))))

(defun <transaction-approved> (approved)
  (<> (div :class (cat "form-check"
		       (unless (ecm/user:user-is-administrator-p)
			 " disabled")))
    (unless (ecm/user:user-is-administrator-p)
      (<>  (input :type "hidden" :name "transaction-approved"
		  :value (if approved "true" "")))
    (<> (input :class "form-check-input"
	       :name (if (ecm/user:user-is-administrator-p)
			 "transaction-approved"
			 "transaction-approved-disabled")
	       :type "checkbox" :value "true"
	       :style "width:25px;height:25px;margin-left:45%"
	       (when approved "checked")
	       (unless (ecm/user:user-is-administrator-p)
		 " disabled"))))))
	       
(defun <transaction-amount> (amount)
  (<> (div :class "input-group")
      ;;(<>(div :class "input-group-addon")"$")
      (<> (input :type "text" :class "form-control" :placeholder "0.00"
	               :name "transaction-amount"
	               :value amount))))

(defun <transaction-limit-of-cover> (amount)
  (<> (div :class "input-group")
      (<>(div :class "input-group-addon")"$")
      (<> (input :type "text" :class "form-control" :placeholder "0.00"
	               :name "transaction-limit-of-cover"
	               :value amount))))

(defun <transaction-types-select> (&key (selected nil))
  (<> (html5:select :name "transaction-type"
		    :class "form-control")
    (dolist (type (ecm/entity/transaction:transaction-types))
      (<> (html5:option :value type
			(when(equalp type selected)
			  (list :selected t)))
	(<> :text type)))))

(defun <transaction-expense-types-select> (&key (selected nil))
  (<> (html5:select :name "transaction-expense-type"
		    :class "form-control")
    (<> (option :value "") "")
    (dolist (type (ecm/entity/transaction:transaction-expense-types))
      (<> (html5:option :value type
			(when(equalp type selected)
			  (list :selected t)))
	(<> :text type)))))

(defun <transaction-headings-select> (&key (selected nil))
  (<> (html5:select :name "transaction-heading"
		    :class "form-control")
    (dolist (type (ecm/entity/transaction:transaction-headings))
      (<> (html5:option :value type
			(when (equalp type selected)
			  (list :selected t)))
	(<> :text type)))))


(defun <transaction-datepicker> (&key (date nil) (name "transaction-date"))
  (<> (input :type "text" :name name
	           :class "datepicker form-control"
	           :size 26
	           :value (if date
			              (if (equalp date "") "" (ecm/ui/utility:format-timestring date))
			              (ecm/ui/utility:format-timestring
			               (ecm/local-time:format-rfc3339-timestring
			                t (ecm/local-time:universal-to-timestamp
			                   (get-universal-time)))))))
  (<> 'html5:script
    (ps:ps
      ($(lambda ()
	        (ps:chain
	         ($ ".datepicker")
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
 

(defun <edit-transaction-modal> ()
  (<> (script)
    (ps:ps
      (defun refresh-transaction ()
	      (alert "refresh!"))
      (defvar transaction ({})))
    (ps:ps* 
     `($ (lambda ()
	         (let* ((modal ($ "#ecmModal"))
		              (title ($. modal (find ".modal-title")))
		              (body ($. modal (find ".modal-body")))
		              (div ($ "#claimTransaction")))

	           (defun show-edit-transaction (id claim &optional (page-title "Update Transaction"))
	             (let ((iframe (+ "<iframe id=\"editTransaction\" name=\"editTransaction\" src=\"/ecm/transaction/" id "/edit/inline\" style=\"border:none;width:100%;height:100%\"></iframe>")))
		             ($. title (empty)
		                 (append (+
			                        "<div style=\"width:100%\" class=\"text-xs-center\"> <h1>"
			                        page-title " for Claim #" claim"</h1>"
			                        "</div>")))
		             ($. body (empty)
		                 (append "<div id=\"editTransactionIFrameLoading\" style=\"font-size:2000%;width:100%\" class=\"text-xs-center\"><i  class=\"fa fa-spinner fa-spin spin-normal\"></i></div>") (append iframe))
		             ($. modal (modal "show"))
		             ($. body (find "iframe")
		                 (on "load" (lambda ()
			                            ($. body (find "#editTransactionIFrameLoading") (hide)))))))

	           (defun show-create-transaction (claim &optional (page-title "Create Transaction"))
	             (let ((iframe (+ "<iframe id=\"createTransaction\" name=\"createransaction\" src=\"/ecm/claim/" claim "/transaction/create/inline\" style=\"border:none;width:100%;height:100%\"></iframe>")))
		             ($. title (empty)
		                 (append (+
			                        "<div style=\"width:100%\" class=\"text-xs-center\"> <h1>"
			                        page-title " for Claim #" claim"</h1>"
			                        "</div>")))
		             ($. body (empty)
		                 (append "<div id=\"createTransactionIFrameLoading\" style=\"font-size:2000%;width:100%\" class=\"text-xs-center\"><i  class=\"fa fa-spinner fa-spin spin-normal\"></i></div>") (append iframe))
		             ($. modal (modal "show"))
		             ($. body (find "iframe")
		                 (on "load" (lambda ()
			                            ($. body (find "#createTransactionIFrameLoading") (hide))
                                  (let ((thsi this))
                                    ($ (lambda ()
                                         ($.  "#createTransaction"
                                              (on "load reload"
                                                  (lambda () 
                                                    ($. "#createTransaction"
                                                        (css "height"
                                                             ($. "#createTransaction"
                                                                 (parent) (height)))))))))))))))

	           (defun hide-edit-transaction ()
                                        ;($. title (empty) )
                                        ; ($. body (empty) )
	             ($. modal (modal "hide")))
	     
	           (let ((body ($ "html, body")))
	             (defun scroll-to-transaction (id)
		             ($. body 
		                 (animate ({}
				                        scroll-Top (- ($. (+ "[data-transaction-id=" id "]")
						                                      (offset) top)
					                                    70))
			                        1000))))
  
	           (setf transaction ({} "show_edit" show-edit-transaction
				                           "hide_edit" hide-edit-transaction
				                           "create" show-create-transaction
				                           "scroll" scroll-to-transaction
				                           "modal" modal
				                           "title" title
				                           "table" ($ "#ecmClaimTransactionTable")))))))))

(defun <claim-transaction-table> (claim-transactions
				  &rest args
				  &key
				    (script t)
				    (claim-id t)
				    (id "ecmClaimTransactionTable")
            (show-create-and-update? (and (or t)) ))
  (<transaction-style> :id id)
  (<edit-transaction-modal>)
  
  ;(break "~A" :here)
  (<> (table :class "table table-sm compact persist-area  mx-auto"
	     :style "background-color:white;margin:auto"
	     :data-persistent-class "floatingHeader floatingTr"
	     :data-persistent-header ".persist-header"
	     :id id
	     :cellspacing "0")
    (<> (thead :class "thead-dark")
      (<> (tr :class "persist-header")
        
	(when claim-id
	  (<> 'th (<> :unescaped "Claim&nbsp;#")))

	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Date")
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Type")
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Heading")
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Amount")
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Expense")
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Approved")
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Payee")
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Cheque Number")
    
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Reference Number")
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Schemes Advance Number")
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Recipient")
		(<> (th :class "text-xs-center"
				:style "padding-right: 0px;")
		  "Date Paid")
  (<> (th :class "text-xs-center"
		      :style "padding-right: 20px;")
	  (<> 'style
	    " .transaction-buttons a { width: 100%; margin-bottom:5px;} ")
	  (<> '(html5:div :class "center-block transaction-buttons align-middle"
		      :style "text-align:center; padding-top:2.5px; padding-left:0.5rem ;width: 100%;")
		  (<> '(html5:div :class "btn-group text-xs-center")
	      (<> `(html5:a :class "btn btn-light btn-sm btn-outline-secondary"
			                :id "tranCreateNew"
			                :href ,(concatenate
				                      'string
				                      "/ecm/create?create[type]=claim-transaction&create[key]=claim-id&access[type]=claim"
				                      "&access[id]=" (princ-to-string claim-id)
				                      "&back[active-tab]=Transactions"))				    
		      (<> "New")))))
	))
	(<> 'tbody
	  (dolist (tr claim-transactions)
	    (apply #'<claim-transaction-tr> tr args))))
  (<> (script) '|
     function createTranCreateNew() {
  $("#tranCreateNew" ).click(function( event ) {
      event.preventDefault();
      transaction.create(|(princ-to-string claim-id)'|);
     });

}
   $(function(){ 

     $("#tranCreateNew" ).click(function( event ) {
      event.preventDefault();
      transaction.create(|(princ-to-string claim-id)'|);
     }); 
     $(".tranny-edit" ).click(function( event ) {
      event.preventDefault();
      transaction.show_edit($(this).data("transaction-id"), $(this).data("claim-id"));
     }); 
         });|)
  (when script
    (<transaction-script> :id id)
    (when (ecm/user:user-is-administrator-p)
      (<transaction-edit-script>))))
  

(defun <claim-transaction-tr> (transaction
			                         &key 
				                         (claim-id t)
				                       &allow-other-keys)
  (let ((claim-id (when claim-id (getjso "claim_id" transaction)))
	      (_id (getjso "_id" transaction))
	      (type (getjso "type" transaction))
	      (date (getjso "date" transaction))
	      (heading (getjso "heading" transaction))
	      (amount (getjso "amount" transaction))
	      (limit-of-cover (getjso "limit_of_cover" transaction))
	      (expense-type (getjso "expense_type" transaction))
	      (approved (ecm/json:from-json-bool
		               (getjso "approved" transaction)))
	      (payee (ecm/json:getjso* "cheque.payee" transaction))
	      (recipient (ecm/json:getjso* "cheque.recipient" transaction))
	      (cheque-number (ecm/json:getjso* "cheque.cheque_number" transaction))
	      (reference-number (ecm/json:getjso* "cheque.reference_number" transaction))
	    (schemes-advance-number (ecm/json:getjso* "cheque.schemes_advance_number" transaction))
	    (date-paid (getjso "date_paid" transaction)))
    (<> (tr :data-transaction-id _id)
      
      (when claim-id
	      (<> (td :class "text-xs-center")
	        (<> :text claim-id)))
      (<> (td :class "text-xs-center")
	      (<> :text date))
      (<> (td :class "text-xs-center")
	      (<> :text type))
      (<> (td :class "text-xs-center")
	      (<> :text heading))
      (<> (td :class "text-xs-center"
	            :style "padding-left:0.5rem;padding-right:0.5rem")
	      (<> :text "$"amount))
      (<> (td :class "text-xs-center")
	      (when expense-type
	        (<> :text expense-type))

        (when limit-of-cover 
	         (<> :text "Limit Of Cover: $"limit-of-cover)))
      (<> (td :class "text-xs-center"
	            :style "font-size : 200%")
	      (when (ignore-errors
		           (or (string= "Cash Call/Scheme Advance" type)
		               (string-equal type "Cheque" :end1 6)))
	        (<> (i :class (if approved
			                      "alert alert-success fa fa-check-square-o"
			                      "alert alert-info fa fa-times-circle-o")
		             :data-ecm-edit-field "approved"
		             :data-ecm-edit-value (if approved "true" "false")
		             :data-transaction-id _id))
	        (<> (span :style "display:none")
	          (<> :text (if approved "true" "false")))))
      (<> (td :class "text-xs-center")
	      (when payee
	        (<> :text
	          (ecm/entity/corpus:corpus-name-as-string
	           payee))))
      (<> (td :class "text-xs-center")
	      (when cheque-number
	        (<> :text cheque-number)))
      (<> (td :class "text-xs-center")
	      (when reference-number
	        (<> :text reference-number)))
      (<> (td :class "text-xs-center")
	      (when schemes-advance-number
	        (<> :text schemes-advance-number)))
      (<> (td :class "text-xs-center")
	    (when recipient
	      (<> :text
	        (ecm/entity/corpus:corpus-name-as-string
	         recipient))))
	  (<> (td :class "text-xs-center")
	    (when date-paid
	      (<> :text date-paid)))
      (<> (td :class "text-xs-center")
	      (<transaction-buttons> _id claim-id)))))


(defun <transaction-buttons> (_id claim-id)
  (<> '(html5:div :class "w-100 text-center btn-group transaction-buttons")
    (<> '(html5:div :class "btn-group mx-auto" :role "group")
      (<> (html5:button :type "button"
                        :class "btn btn-secondary btn-sm dropdown-toggle"
                        :data-toggle "dropdown")
        (<> (i :class "fa fa-bars") " "))
      (<> '(html5:div :class "dropdown-menu")
        ;; Edit
        (when (ecm/user:user-can-edit-p)
	        (<> `(html5:a :class "dropdown-item tranny-edit"
		                    :href ,(concatenate
				                        'string
				                        "/ecm/edit?claim-transaction="
				                        (princ-to-string _id)
				                        "&go-back-to-claim="
				                        (princ-to-string claim-id)
				                        "&access[type]=claim&access[id]="
				                        (princ-to-string claim-id))
		                    :data-transaction-id ,(princ-to-string _id)
		                    :data-claim-id ,(princ-to-string claim-id))
	          (<> "Edit")))
        ;; Delete
        (when (ecm/user:user-is-administrator-p)
	        (<> `(html5:a :class "dropdown-item"
			                  :href ,(concatenate
				                        'string
				                        "/ecm/delete?claim-transaction="
				                        (princ-to-string _id)
				                        "&go-back-to-claim="(princ-to-string claim-id)
				                        "&access[type]=claim&access[id]="
				                        (princ-to-string claim-id)))
		        (<> "Delete")))))))

(defun ps/transaction-approved ()
  (ps:ps 
    (ps:labels 
	      ((toggle (i status)
	         (ps:chain
	          ($ i) 
	          (toggle-class "alert-success fa-check-square-o"
			                    status))
	         (ps:chain
	          ($ i) (toggle-class "alert-info fa-times-circle-o"
				                        (not status))))
	       (ajax (i data status)
	         (console.log data)
	         (let ((res |data.responseJSON|))
	           (if (and (not (eq res ps:undefined))
		                  (ps:chain res (has-own-property "approved")))
		             (progn (toggle i res.approved))
		             (alert "error")))))
			   		      	       
      (ps:chain 
       ($ "[data-ecm-edit-field=approved]")
       (click (lambda ()
		            (ps:var i this)
		            (ps:var transaction-id 
			                  (ps:chain ($ i) (data "transaction-id")))
		            #+(or)(alert (+ "/ecm/transaction/" transaction-id 
			                          "/toggle-approved"))
		            ($.ajax (+ "/ecm/transaction/" transaction-id 
			                     "/toggle-approved")
			                  (ps:create 
			                   "dataType" "json"
			                   "complete" (lambda (d s)
				                              (ajax i d s))))))))))

(defun <transaction-edit-script> (&key (id "ecmClaimTransactionTable"))
  (declare (ignorable id))
  (<> (html5:script)
    '|$(document).ready(function() {

$('[data-ecm-edit-field=approved]').hover(
  function() {

// first, append the pencil
    $( this ).append( $( '<div class="div-to-display"> <i class="fa fa-pencil-square-o" aria-hidden="true"></i></div>')); 

    
    $(this).css('cursor','pointer');

  }, function() {
    $( this ).find( "div:last" ).remove(); 
    $(this).css('cursor','auto');
  }
);

// Now the click handler

| (ps/transaction-approved) '|

// /ready
})
|))

(defun <transaction-script> (&key (id "ecmClaimTransactionTable"))
  (<> (html5:script)
    
    '|function transactionDatatable()  {
    var table = $('#|
    id '|').DataTable({
       
        "paging" : false,
        "fixedHeader" :{
            header: true,
            footer: false
        },
        "autoWidth": false,
       responsive: {
            details: {
                display: $.fn.dataTable.Responsive.display.childRowImmediate,
                type: 'none',
                target: ''
            }
        },

        "columnDefs" : [ { orderable: false, targets: [11] },
                         { "visible": false, "targets": [0, 12, 11,10,9,8,7]} ],
        "drawCallback": function ( settings ) {
            var api = this.api();
            var cols = api.columns([7,8,9,10,11,12]).data()
            var col = cols[0];
            var rows = api.rows().nodes()

            console.debug('Drawing table with cheque', cols)

            $(col).each( function (n,data) { 
               var payee = cols[0][n] ;
               var cheque_number = cols[1][n] ;
               var reference_number = cols[2][n] ;
               var schemes_advance_number = cols[3][n] ;
               var recipient = cols[4][n] ;
               var date_paid = cols[5][n] ;

            console.debug('date paid', date_paid)

             if (payee != "" \|\| cheque_number != ""
                 \|\| reference_number != "" 
                 \|\| schemes_advance_number != "") { 

//               console.log('n:' + n + '\\npa:' + payee +
  //                           '\\ncn' + cheque_number) ;
    //           console.log($(rows).eq( n ) );
      //         console.log($(rows).eq( n ).find('td').length );

              payee = payee ? '<tr data-transaction-cheque="true"><th style="border-top:none;">Payee</th><td style="border-top:none;">'+payee+'</td></tr>' : '' ;
              cheque_number = cheque_number ? '<tr><th style="border-top:none;">Cheque Number</th><td style="border-top:none;">'+cheque_number+'</td></tr>' : '' ;
              reference_number = reference_number ? '<tr><th style="border-top:none;">Reference Number</th><td style="border-top:none;">'+reference_number+'</td></tr>' : '' ;
              schemes_advance_number = schemes_advance_number ? '<tr><th style="border-top:none;">Schemes Advance Number</th><td style="border-top:none;">'+schemes_advance_number+'</td></tr>' : '' ;
              recipient = recipient ? '<tr><th style="border-top:none;">Recipient</th><td style="border-top:none;">'+recipient+'</td></tr>' : '' ;
             date_paid = date_paid ? '<tr><th style="border-top:none;">Date Paid</th><td style="border-top:none;">'+date_paid.replace(/T/, ' ')+'</td></tr>' : '' ;



              $(rows).eq( n ).after(
                  '<tr><td></td><td colspan="5">'
                   + '<div class="cheque"><table class="table table-sm">'
                   + payee + cheque_number + reference_number + schemes_advance_number + recipient + date_paid
                   +' </table></div></td>'
                   + '<td colspan "1"></td></tr>' ) ;

              var this_row = $(rows)[n] ;
   
              $(this_row).next().find('td:first').css('border-left', '1px solid lightgray');
              $(this_row).next().find('td:last').css('border-right', '1px solid lightgray');
              $(this_row).next().find('td').css({"margin-bottom": '0.5em', "border-top" : "none", "border-bottom" : "1px solid lightgray"});
              $(this_row).find('td:first').css('border-left', '1px solid lightgray');
              $(this_row).find('td:last').css('border-right', '1px solid lightgray');
              $(this_row).find('td').css({"padding-top": '0.5em', "border-top" : "1px solid lightgray", "border-bottom" : "none"});


              }
               

            });

        }
   ///datatable
    });
    
      transaction.table = $("#ecmClaimTransactionTable");
      return table;
       
				     
    }
    $(document).ready(function () {
     var transaction_table = transactionDatatable();
        
     $('#ecmClaimTransactionTable_filter').first().prepend($('#tranCreateNew').clone());
     createTranCreateNew();
 // /ready
  })
|))




