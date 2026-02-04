(defpackage :ecm/ui/report/time-recorded
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.| #:$.)
  (:import-from :ecm/api/find)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/ui/spreadsheet
		#:<spreadsheet-type-select>
		#:<download-spreadsheet>)
  (:import-from :ecm/user)
  (:import-from :ecm/ui/navbar))
(in-package  :ecm/ui/report/time-recorded)

(defun <timecards> (list id claim-number interval user-name)
  (<> `(div :id ,id
	    :class "modal fade")
    (<> '(div :class "modal-content")
      (<> '(div :class "modal-header")
      (<> :unescaped
	'|
        <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
      |)
      (<> '(h4 class="modal-title")
	(<> :text "Time Recorded")
	  (<> '(small)
	    (<> '(span :class "text-muted") " by ")
	    (<> :text user-name)
	    (<> '(span :class "text-muted") " on #")
	    (<> :text claim-number)
	    (<> '(span :class "text-muted") " for ")
	    (<> :text interval))))
      (<> '(table :style "width:100%" :class "table" )
	(<> 'thead
	  (<> 'tr (dolist (h '("Time" "Hours" "Notes" "Mileage" "Disbursements"))
		    (<> '(th) (<> :text h)))))
	(<> 'tbody
	(dolist (tc list)
	  (<> 'tr
	    
	    (ecm/json:mapjso
	     (lambda (k v)
	       (unless (find k '("_id" "_type" "claim_id" "app_user_id")
			     :test #'string-equal)
		 (<> '(td)
		   (cond ((string= k "date")
			  (<> '(span :class "text-nowrap")
			    (<> :text (substitute #\Space #\T v))))
			 ((string= k "notes")
			  (<> '(div)
			    (<> 'span (<> :text v))))			 
			 (t (<> :text v))))))
	     tc))))))))
  

(defun time-recorded-report/post (app-user-id start-time end-time)
  (let* ((json (postmodern:query
		(concatenate
		 'string
		 "SELECT json_app_user_claim_time_recorded("
		 (princ-to-string app-user-id)
		 ", '"start-time"', '"end-time"')")
		:single))
	 (jso (if (eq :null json) json
		  (ecm/json:read-json-from-string json))))
    
    (if (eq :null jso)
	(<> "No Time Recorded")
	(let* ((claims (ecm/json:getjso "Claims" jso))
	       (interval (ecm/json:getjso* "Report Period.interval" jso))
	       (user-name (ecm/json:getjso* "User Name" jso)))
	  (let ((n 0))
	    (flet ((n-id (&optional start)
		     (concatenate '
		      string start "modal-" (princ-to-string n))))
	      (dolist (c claims)
		(incf n)
		(<timecards> (ecm/json:getjso "Timecards" c) (n-id)
			     (ecm/json:getjso "Claim Number" c)
			     interval user-name))))
	  (<> 'h3 (<> :text "Time Recorded")
	      (<> '(small)
		(<> '(span :class "text-muted") " by ")
		(<> :text user-name)
		(<> '(span :class "text-muted") " for ")
		(<> :text interval)))
	  (<> '(table :style "width:100%" :class "table")
	    (<> 'thead
	      (<> 'tr (ecm/json:mapjso
		       (lambda (k v)
			 (declare (ignore v))
			 (<> '(th)
			   (<> :text k)))
		       (first claims))))

	    (let ((n 0))
	      (flet ((n-id (&optional start)
		       (concatenate '
			string start "modal-" (princ-to-string n))))
		(dolist (c claims)
		  (incf n)
		  (<> 'tr
		    (ecm/json:mapjso
		     (lambda (k v)
		       (<> '(td)
			 (cond((string-equal k "Timecards")
			       (<> `(button :data-toggle "modal"
					    :data-target ,(n-id "#"))
				 (<> :unescaped
				   '|<i class="fa fa-angle-down"></i>|))

			       )
			      ((string-equal k "Claim Number")
			       (<> `(a :href ,(concatenate
					       'string "/ecm/view?claim="
					       (princ-to-string v))
				       :target "_blank")
				 (<> :text v)))
			      (t (<> :text v)))))
		     c))))))))))
    
	    

(hunchentoot:define-easy-handler
    (time-recorded-report-handler
     :uri "/ecm/report/time-recorded")
    (start-time end-time)
  (if (string-equal (hunchentoot:request-method*) "POST")
      (<> (ecm/ui/page:page)
	(ecm/user:with-user ()
	  (time-recorded-report/post (ecm/user:user-id
				      (ecm/user:user))
				   start-time
				   end-time)))
      (<> '(ecm/ui/page:page :title "Time Recorded : ECM")
	(<> '(html5:h1 :style "text-align:center;width:100%")
	  "Time Recorded")
	(<> 'ecm/ui/navbar:navbar)
	(<> 'style 
          (<> "/* centered columns styles */
.row-centered {
    text-align:center;
}
.col-centered {
    display:inline-block;
    float:none;
    /* reset the text-align */
    text-align:left;
    /* inline-block space fix */
    margin-right:-4px;
}"))
	(<time-recorded-report-form>)
	(<> 'style
          (<> "iframe {
    border: none;
    resize: both;
  overflow: auto;
}"))
	(<> '(iframe :name "reportFrame"  
	      :id "reportFrame"
	      :style "overflow: scroll; width:100%; height:100%")
	  " ")

		  (<> 'html5:script	    
	    "
$(\"#reportFrame\").load(function() {
    $(this).height( $(this).contents().find(\"body\").height() );
});"))))

(defun <time-recorded-report-form> ()
  (<> '(form :method "POST" 
        :action "/ecm/report/time-recorded"
        :target "report-frame")
    (<> 'style
      (<> :unescaped "input[type=\"text\"]
{
    width:100%;
    text-align:center
}"))
    (<> 'html5:script
      (ps:ps
	($(lambda ()
	    (ps:chain
	     ($ ".datepicker")
	     (datetimepicker
	      ({}
			"changeMonth" t
			"orientation" "bottom"
			"showTimezone" t
			"timeFormat" "HH:mm:ss Z"
			"timeInput" t
			"dateFormat" "D, d M yy"
			"maxDate"  0
			"controlType" "select"
			"oneLine" t
			#+(or)"onSelect" #+(or)(lambda ()
						 (console.log this)			       
						 (let ((inputs
								 ($. this
									 (closest "form")
									 (find ":input"))))
						   (|.| inputs (eq (+ (inputs.index this) 1))
								(focus))
						   (|.| ($ ".datepicker")
								(datetimepicker "hide"))))
			;;"hourMax" (ps:chain (moment) (hour))

			)))))))
    (<> '(div :class "row row-centered")

      (<> '(div :class "col-md-6 col-centered")
        (<> '(div :class "row-centered")
          (<> 'h3 (<> "Start Time (including)"))
          (<> '(input :name "start-time"
                :class "datepicker"
                :id "start-time"
		:type "text"))))
      (<> '(div :class "col-md-6 col-centered")
        (<> '(div :class "row-centered")
          (<> 'h3 (<> "Stop Time (excluding)"))
          (<> '(input :name "end-time"
                :class "datepicker"
                :id "end-time"
		:type "text")))))

	(<> '(div :class "row row-centered report")
      (<> '(span :id "ss-type-span"
			:style "width: 100%"
			:class "row-centered"))
      (<> 'br)
      (<> '(button :type "submit" :class "btn btn-success")
		(<> "View Report")))))




