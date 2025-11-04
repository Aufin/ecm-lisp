(defpackage :ecm/ui/diary
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.|)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/user)
  (:import-from :ecm/entity/claim)
  (:import-from :ecm/json
		#:getjso)
  (:import-from :ecm/ui/navbar)
  (:import-from :ecm/endpoint)
  (:import-from :ecm/entity/user)
  (:import-from :ecm/entity/attachment)
  (:import-from :ecm/entity/corpus
		#:corpus-name-as-string)
  (:import-from :ecm/entity/timecard)
  (:export #:<claim-diary-page>
	   #:diary-entry-page
	   #:<diary-entries-table>))

(in-package :ecm/ui/diary)

(defun <diary-style> ()
  (<> (html5:style)
  ".dtr-title .diary-entry-buttons { display:none;}

.dtr-data .diary-entry-buttons {
  display:inline;
  position: absolute;
  top: -2.25rem;
  left: 1rem;
}

#claimDiaryTable .dtr-title {
 display:inline;
}

#claimDiaryTable .alert {

  z-index: 1024;
  opacity: 0.8;

}
  "))

(defun <diary-entry-buttons> (_id claim-id processed user-id)
  (<> '(html5:div :class "center-block diary-entry-buttons")
    (<> '(html5:div :class "btn-group text-center mx-auto")
      (<> (html5:button :type "button"
                        :class "btn btn-secondary btn-sm dropdown-toggle"
                        :data-toggle "dropdown")
        (<> (i :class "fa fa-bars") " "))

      (<> '(html5:div :class "dropdown-menu")
      (<> `(html5:a :class "dropdown-item"
		                :href ,(concatenate
			                      'string
			                      "/ecm/view?diary_entry="
			                      (princ-to-string _id)))
	      (<> "View"))
      (unless (or processed (ecm/user:user-read-only-p)
		  (and (not (ecm/user:user-is-administrator-p))
		       (not (equal (ecm/user:user-id)
				   user-id))))
				   
	      (<> `(html5:a :class "dropdown-item"
			    :href ,(concatenate
				    'string
				    "/ecm/edit?diary-entry="
				    (princ-to-string _id)
				    "&future[processed]=T&access[type]=claim"
				    "&access[id]=" (princ-to-string claim-id)
				    "&go-back-to-claim="(princ-to-string claim-id)))
		(<> "Mark as Done"))
	    (<> `(html5:a :class "dropdown-item"
			  :href ,(concatenate
				  'string
				  "/ecm/create?create[type]=defer-diary-entry"
				  "&create[key]=diary-entry-id"
				  "&access[type]=diary-entry"
				  "&access[id]=" (princ-to-string _id)
				  "&go-back-to-claim="(princ-to-string claim-id)))
	      (<> "Defer")))
	    (when (ecm/user:user-is-administrator-p)
	      (<> `(html5:a :class "dropdown-item"
			    :href ,(concatenate
				    'string
				    "/ecm/edit?diary-entry="
				    (princ-to-string _id)
				    "&go-back-to-claim="
				    (princ-to-string claim-id)
				    "&access[type]=claim&access[id]="
				    (princ-to-string claim-id)))
		(<> "Edit"))
	      (<> `(html5:a :class "dropdown-item"
			    :href ,(concatenate
				    'string
				    "/ecm/delete?diary-entry="
				    (princ-to-string _id)
				    "&go-back-to-claim="
				    (princ-to-string claim-id)
				    "&access[type]=claim&access[id]="
				    (princ-to-string claim-id)))
		(<> "Delete")))))))

(defun <diary-entry-tr> (diary-entry
			 &key (claim-number t)
			   (claim-id)
			   (schedule t)
			   (deadline t)
			   (deferred t)
			   (status t)
			   (user t)
			   (note t))
  (let* ((schedule (when schedule (getjso "schedule" diary-entry)))
	 (deadline (when deadline (getjso "deadline" diary-entry)))
	 (display-deferred deferred)
	 (deferred (when deferred (getjso "deferred" diary-entry)))
	 (processed (when status (ecm/json:from-json-bool
				  (getjso "processed" diary-entry))))
	 (outstanding (when status (ecm/json:from-json-bool
				    (getjso "outstanding" diary-entry))))
	 (_id (getjso "_id" diary-entry))
	 (claim-id (or (when (integerp claim-id) claim-id)
		       (getjso "claim_id" diary-entry)))
	 (user (when user (getjso "user" diary-entry)))
	 (note (when note (getjso "note" diary-entry))))
    (<> (tr :style "border-bottom: none;")
      (when claim-number
	(<> 'td (<> :text claim-id)))
      (when status
	(let ((alert
	       (cond (outstanding "alert alert-danger fa fa-times-circle-o")
		     (processed "alert alert-success fa fa-check-square-o")
		     (t "alert alert-info fa fa-square-o"))))
	
	  (<> (td :style "font-size : 150%"
		  :class " text-center")
	    (<> (i :class  alert :style "padding:5px;")
	      (<> (span :style "display:none")
		(<> :text (cond (outstanding "outstanding")
				(processed "processed")
				(t "standing"))))))))
      (when note
	(<> 'td
	  #+(or)(when outstanding
		  (<> '(html5:span :class "alert alert-danger"
			:style "float:left; margin-right:10px")
		    (<> :text "Outstanding Diary Entry")))
	  (<> (blockquote :class "blockquote")
	    (<> :text
	      note))))
      (when schedule
	(<> 'td (<> :text schedule)))
      (when deadline 
	(<> (td :class " text-center")
	  (<> '(div :style "min-width: 6.5em")
	    (<> :text deadline))))
      (when display-deferred
	(<> '(td :class "text-xs-left")
	  (when deferred
	    (<> (ul :class "list-unstyled")
	      (dolist (d deferred)
		(<> (li)
		  (<> :text d)))))))
      (when user 
	(<> (td :class "text-center")
	  (<> :text
	    (ecm/entity/corpus:corpus-name-as-string
	     (getjso "corpus" user)
	     :company-name nil
	     :province nil))))
      (<> (td :class "text-center")
	(<diary-entry-buttons> _id claim-id processed
			       (getjso "_id" user))))))


(defun <diary-entries-table> (diary-entries
			                        &rest args
			                        &key
				                        (claim-id)
				                        (id "claimDiaryTable")
				                        (claim-number t)
				                        (schedule t)
				                        (deadline t)
				                        (deferred t)
				                        (status t)
				                        (user t)
				                        (note t))
  (<diary-style>)
  (<> (table :class "table table-striped compact persist-area"
	           :style "background-color:white"
	           :data-persistent-class "floatingHeader floatingTr"
	           :data-persistent-header ".persist-header"
	           :id id
	           :cellspacing "0"
	           :width "100%")
    (<> (thead :class "thead-dark")
	    (<> (tr :class "persist-header")
	      (when claim-number
	        (<> 'th (<> :unescaped "Claim&nbsp;#")))
	      (when status
	        (<> (th :class "text-center")
	          "Status"))
	      (when note (<> (th :class "text-center" ) "Note"))
	      (when schedule (<> (th :class "text-center") "Schedule"))
	      (when deadline (<> (th :class "text-center") "Deadline"))
	      (when deferred (<> (th :class "text-center") "Deferred"))
	      (when user
	        (<> (th :class "text-center")
	          "User"))
	      (<> (th :class "text-center")
	        (<> 'style
	          " .diary-entry-buttons a { width: 100%; margin-bottom:5px;} ")
	        (<> '(html5:div :class "center-block diary-entry-buttons"
		            :style "text-align:center; padding-top:2.5px; width: 100%;")
		        (<> '(html5:div :class "btn-group text-center"
		              :style "width: 80%; min-width:100px;")
	            (<> `(html5:a :class "btn btn-light btn-sm btn-outline-secondary"
                            :id "diaryCreateNew"
			                      :href ,(concatenate
				                            'string
				                            "/ecm/create?create[type]=diary-entry&create[key]=claim-id&access[type]=claim"
				                            "&access[id]=" (princ-to-string claim-id)
				                            "&back[active-tab]=Diary-Entries"))
		            (<> "New")))))))
	  (<> 'tbody
	    (dolist (de (getjso "entries" diary-entries))
	      (apply #'<diary-entry-tr> de args))))
  (<> 'html5:script
	  "$(document).ready(function() {
     var diary_table = $('#" id '|').DataTable(
     {
      "paging" : false,
      "columnDefs" : [ { orderable: false, targets: [4] }],
      "autoWidth": false,
       responsive: {
          details: {
              display: $.fn.dataTable.Responsive.display.childRowImmediate,
              type: 'none',
              target: ''
          }
      }
    });

  $('#claimDiaryTable_filter').prepend($('#diaryCreateNew').clone());
});|))					;

(defun <claim-diary-page> (claim-id)
  (<> '(ecm/ui/page:page
	:title "Claim Diary"
	:ecm-body-class "")
    (<diary-entries-table>
     (ecm/entity/claim:claim-diary claim-id)
     :claim-id claim-id
     :claim-number nil
     :schedule nil
     :deferred nil)))

(defmacro <diary-form> (&body body)
    `(<> '(html5:form
	   :method "POST")
       (<> 'html5:script
       "$.datetimepicker.setDateFormatter({
    parseDate: function (date, format) {

/*     console.log('parse f:' + format); */
        var d = moment(date, format);
        return d.isValid() ? d.toDate() : false;
    },
    
    formatDate: function (date, format) {
       var new_format ;

      if (format[0] == 'H') {
         new_format = 'H:mm' ;
    } else {
       new_format = format ;
    }

/*     console.log('f:' + format + ' n:' + new_format); */
        return moment(date).format(new_format);
    }
});
"
       (ps:ps
	 ($ (lambda () 
	      (ps:chain ($ ".datepicker")
			(datetimepicker
			 ({} "format" "YYYY-MM-DD HH:mm:ssZ"
			     "maxDate" (|.|
					(moment)
					(format "YYYY-MM-DD"))			     
			     "closeOnDateSelect" t)))))))
       ,@body))

(defun <claim-create-diary-page> (claim-id))

(defun diary-entry-page (&rest foo))
  
