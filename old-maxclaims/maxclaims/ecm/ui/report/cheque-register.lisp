(defpackage :ecm/ui/report/cheque-register
  (:use :cl)
  (:import-from :ecm/user #:with-user)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.|)
#+(or)  (:import-from :ecm/ui/spreadsheet
		#:<spreadsheet-type-select>
		#:<download-spreadsheet>)
  (:import-from :ecm/ui/report/agency-bordereau
		#:list-agencies)
  (:import-from :ecm/entity/corpus
		#:corpus-name-as-string)
  (:export #:agency-cheque-register-page
	   #:agency-cheque-register-report-page
	   #:cheque-register-report-page
	   #:contract-cheque-register-page))
(in-package :ecm/ui/report/cheque-register)

(defun cheque-register-page
    (&key (error nil)
       (title "Agency Cheque Register")
       (action "/ecm/cheque-register/agency")
       (h1 title)
       (contract-id)
       (agency-id)
       (start-time)
       (end-time)
       (risk )
       (select-thunk
	(lambda () (<> (div :class "row is-table-row")
		     (<> (div :class "col-md-4")
		       (<> 'h3 "Agency"))
		     (<> (div :class "col-md-8"
			      :style "font-size:125%")
		       (<> '(select :name "agency-id"
			     :style "width:100%")
			 (dolist (s (list-agencies))						   
			   (<> (option :value (second s)
				       (when (equalp  (second s)
						      agency-id)
					 (list :selected "selected")))
			     (<> :text (first s))))))))))
  (<> (ecm/ui/page:page :title title)
    (<cheque-register-navbar> h1)
    (<> (form :action action
	      :method "POST")

      
      (<> 'style "
.chosen-container-single .chosen-drop {
    z-index: 424242;
 }


.is-table-row {

    }
    .is-table-row [class*=\"col-\"] {
     
        vertical-align: middle;
        margin-bottom: 10px;
    }
html, body, .container-table {
    height: 100%;
    background-color: lightgray;

}
.className {
        background-color: white;   ;
}
.vertical-center-row {
    display: table-cell;
    vertical-align: middle;
}
")
      (<> 'html5:script
	"$(function() { 

  $('select').chosen() ;
    $('.className').css({
        'position' : 'absolute',
        'left' : '50%',
        'top' : '30%',
        'border': '1px solid black',
         'padding' : '20px',
        'margin-left' : -$('.className').outerWidth()/2,
        'margin-top' : -$('.className').outerHeight()/2
    });
});
"
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
		    "oneLine" t)))))))
      (<> '(div :class "className container")
	(when error 
	  (<> (div :class "row is-table-row")
	    (<> (div :class "col-md-4")
	      (<> 'h3 "Error"))
	    (<> (div :class "col-md-8 alert alert-danger"
		     :style "font-size:125%")
	      (<>  (pre) (<> (code) (<> :text error))))))
	(funcall select-thunk)
	(<> (div :class "row is-table-row")
	  (<> (div :class "col-md-4")
	    (<> 'h3 "Start Time" (<> 'small " (inclusive)")))
	  (<> (div :class "col-md-8"
		   :style "font-size:125%")
	    (<> (html5:input
		 :class "form-control datepicker"
		 :name "start-time"
		 :type "text"
		 :style "position: relative; z-index: 100000;"
		 :value (if start-time
			    (ecm/ui/utility:format-timestring start-time)
			    (ecm/ui/utility:format-timestring
			     (ecm/local-time:format-rfc3339-timestring
			      t (local-time:encode-timestamp 0 0 0 0 1 1 2018))))))))
	(<> (div :class "row is-table-row")
	  (<> (div :class "col-md-4")
	    (<> 'h3 "End Time" (<> 'small "  (exclusive)")))
	  (<> (div :class "col-md-8"
		   :style "font-size:125%")
	    (<> (html5:input
		 :class "form-control datepicker"
		 :name "end-time"
		 :type "text"
		 :style "position: relative; z-index: 100000;"
		 :value (if end-time
			    (ecm/ui/utility:format-timestring end-time)
			    (ecm/ui/utility:format-timestring
			     (ecm/local-time:format-rfc3339-timestring
			      t (local-time:encode-timestamp 0 0 0 0 1 2 2018))
			     ))))))
	(<> (div :class "row is-table-row")
	  (<> (div :class "col-md-4")
	    (<> 'h3 "Risk"))
	  (<> (div :class "col-md-8"
		   :style "font-size:125%")
	    (<> '(select :name "risk"
		  :style "width:100%")
	      (<> (option :value "")
		(<> "None"))
	      (dolist (s (list-risks))
		(<> (option :value s
			    (when (equalp risk s)
			      (list :selected "selected")))
		  (<> :text s))))))
        (<> '(div :class "row row-centered report")
	  (<> 'br)
	  (<> '(div :class "col-xs-12 col-centered text-xs-center")
	    (<> '(span :id "ss-type-span"
		  :class "row-centered"))
	    (<> 'hr)
	    (<> '(button :type "submit" :class "btn btn-success")
	      (<> "Run Report"))))))))

(defun agency-cheque-register-page (&key (error nil)
				      (agency-id)
				      (start-time)
				      (end-time)
				      (risk ))
  (cheque-register-page
   :agency-id agency-id
   :error error :start-time start-time :end-time end-time
   :risk risk))

(defun contract-cheque-register-page (&key (error nil)
					(contract-id)
					(start-time)
					(end-time)
					(risk ))
  (declare (ignorable contract-id))
  (cheque-register-page
   :action "/ecm/cheque-register/contract"
   :select-thunk
   (lambda ()
     (<> (div :class "row is-table-row")
       (<> (div :class "col-md-4")
	 (<> 'h3 "Contract"))
       (<> (div :class "col-md-8"
		:style "font-size:125%")
	 (<> '(select :name "contract-id"
	       :style "width:100%; position: relative;")
	   (dolist (s (list-contracts))
	     (<> (option :value (getf s :contract-id)
			 (when (equalp (getf s :contract-id)`
				       contract-id)
			   (list :selected "selected")))
	       (<> :text (getf s :contract-number))
	       (let ((ef (ecm/json:null->nil (getf s :effective-date)))
		     (ex (ecm/json:null->nil (getf s :expiry-date))))
		 (when (or ef ex) (<> :text " :: "))
		 (when ef (<> :text " "
			      (ecm/ui/utility:format-timestring
			       ef
			       :format ecm/local-time:+ISO-8601-DATE-FORMAT+)))
		 (when ex (<> :text " to "
			      (ecm/ui/utility:format-timestring
			       ex :format ecm/local-time:+ISO-8601-DATE-FORMAT+))))))))))
   :error error :title "Contract Cheque Register" :start-time start-time :end-time end-time
   :risk risk))

(defun list-risks ()
  (postmodern:query "select type_name from risk_type"
		    :column))

(defun list-contracts ()
  (postmodern:query "select contract_id, contract_number, effective_date, expiry_date
FROM contract
ORDER BY effective_date DESC NULLS LAST, expiry_date DESC NULLS LAST, contract_number"
		    :plists))

(defun <cheque-register-navbar> (h1)
  (<> (ecm/ui/navbar:navbar)
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
  top: 2.75rem; left: 0.25em;
}
")
      
    (<> (div :id "navClaimTitle")
      (<> 'h1 (<> :text h1)))))

(defun <agency-header> (agency risk start-time end-time)
  (<> (table :class "table table-reflow" :style "display:inline-block")
    (<> (thead :class "thead-inverse")
      (<> (tr)
	(<> (th) "Agency")
	(when risk (<> (th) "Risk Type"))
	(<> (th) "Start Time")
	(<> (th) "End Time")))	  
    (<> (tbody)
      (<> (tr)
	(<> (th) (<> :text (corpus-name-as-string agency)))
	(when risk 	(<> (th) (<> :text risk)))
	(<> (th) (<> :text start-time))
	(<> (th) (<> :text end-time))))))

(defun cheque-register-report-page (cheque-register &key (header-thunk (lambda ()))
						      (title "Cheque Register"))
  (<> (ecm/ui/page:page :title title)
    (<> (h1 :style "display:inline-block") (<> :text title))
    (funcall header-thunk)
    (<> (table :class "table table-sm")
      (<> (thead :class "thead-inverse")
	(<> (tr)
	  (loop :for (name . nil) :in (first cheque-register)
	     :do (<> (th) (<> :text name)))))
      (<> (tbody)
	(dolist (row cheque-register)	  
	  (<> (tr)
	    (loop :for (nil . value) :in row
	       :do
	       (<> (td)
		 (if (eql 'simple-date:timestamp
			(class-name (class-of value)))
		     (let* ((string (simple-date:timestamp-to-universal-time value))
			    (string (ecm/local-time:universal-to-timestamp string)))
		       (<> (span :style "white-space: nowrap;")
		       (<> :text (local-time:format-timestring
				  nil string
				  :timezone local-time:+utc-zone+
				  :format ecm/local-time:+iso-8601-date-format+))))
		     (<> :text			  
		       value))))))))))

(defun agency-cheque-register-report-page (cheque-register agency risk start-time end-time)
  (cheque-register-report-page
   cheque-register
   :header-thunk (lambda () (<agency-header> agency risk start-time end-time))
   :title "Agency Cheque Register"))

