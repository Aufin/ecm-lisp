(uiop:define-package :ecm/ui/timecard
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.| #:$.)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/ui/utility
		#:cat
		#:<link-to-viewer>
		#:go-back-to-claim)
  (:import-from :ecm/user)
  (:import-from :ecm/json
		#:getjso)
  (:import-from :ecm/ui/navbar)
  (:import-from :ecm/endpoint)
  (:import-from :ecm/entity/user)
  (:import-from :ecm/entity/claim)
  (:import-from :ecm/entity/attachment)
  (:import-from :ecm/entity/corpus
		#:corpus-name-as-string)
  (:import-from :ecm/entity/timecard)
  (:import-from :ecm/ui/attachment)
  (:export #:create-timecard-page
	   #:timecard-page
	   #:interim-page
	   #:create-or-edit-interim-page))

(in-package :ecm/ui/timecard)

(defparameter +timecard-date-format+
  ;; 2008-11-18 02:32:00.586931+01:00
  (append ecm/local-time:+iso-8601-date-format+ (list #\Space) '((:hour 2) #\: (:min 2) #\: (:sec 2)) (list :gmt-offset-or-z)))
(defun <timecard-style> (&key (id "ecmClaimTimecardTable"))
  (<> (html5:style)
    "

.text-and-attachment {
  position: relative ;
  top: -1rem;
 /*  color:blue;
  background-color:green; */
 background-color:white; 
  box-shadow: 0 2px 4px 0 rgba(0,0,0,0.16),0 2px 10px 0 rgba(0,0,0,0.12)!important;
  border: 1px solid lightgray;
  border-radius: 0.5rem;
  padding: 0.5rem;
  z-index: 100;

}

.btn-group .btn {
  flex:0; margin: auto;
}
td {
    position: relative;
}

/* Responsive datatable */

.dtr-title .timecard-buttons { display:none;}
.dtr-data .timecard-buttons {
    display:inline;
    float: right;
    margin:auto;
    position: relative;
    top: -1.5rem;
   }

#ecmClaimTimecardTable_wrapper .dtr-details li  {
  padding:0px;
  margin-right:0.25rem;
  display:inline;
  border:0px;
}


#ecmClaimTimecardTable_wrapper li {
  border:0px;
  display:inline;
}

#ecmClaimTimecardTable_wrapper .dtr-details .btn-group {

  display: inline;
  margin: auto;
}


 .zold-edit {
    position: absolute ;
    top: -2em;
}
 "
    "
     #"id"  { 
 z-index : -1;

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

td {
    position: relative;
}
td:hover > .div-to-display {
    display: block
}
"))


(defun <timecard-script> (&key (id "ecmClaimTimecardTable"))
  (<> (html5:script)
    (ps:ps

      (defun member (element array)
	(not (equal (ps:chain j-query (in-array element array)) -1)))

      (defun remove (element array)
	(ps:chain array (splice (ps:chain j-query (in-array element array)) 1))

	array)

      
      
      (defun make-interim-border (tr)
	(|.| ($ tr)
	     (find "td:first")
	     (css "border-left" "2px solid lightgrey"))
	(|.| ($ tr)
	     (find "td:last")
	     (css "border-right" "2px solid lightgrey")))
      (defun make-interim-header (interim &optional
					    (name "Interim") (minimize t))
	(when (equal interim._type "timecard-continual")
	  (setf name "Continual"))
	(+ "<tr class=\"interim-header\"><td colspan=\"7\" style=\""
	   "border:2px solid lightgrey ;border-bottom:none; "
	   "border-radius: 0.5rem 0.5rem 0px 0px;\">" 
	   "<div class=\"card\"><div class=\"card-header text-xs-center\" data-interim-id=\""interim._id"\">"
	   "<h5 class=\"d-inline\">" name
	   "<small class=\"text-muted\"> from </small>"
	   (|.| (moment interim.effective_time)
		(format "ddd, DD MMM YYYY HH:mm:ss ZZ"))
	   "<small class=\"text-muted\"> to </small>"
	   (|.| (moment interim.expiry_time)
		(format "ddd, DD MMM YYYY HH:mm:ss ZZ"))
	   " (" (and interim.timecard_ids interim.timecard_ids.length) " cards)"
	   "</h5>"
;	   (console.log minimize)
#+(or)	   (if (not minimize)
	       ""
	       (+ "<i style=\"margin-left:10px;float:right;color:grey;font-size:150%;\" class=\"hover-timecard fa "
	     (if (member (|.| (or interim._id  "continual") (to-string))
			 view_interims)
		 "fa-caret-square-o-up"
		 "fa-caret-square-o-down")
	     "\" data-interim-id=\""(or interim._id "continual")"\"></i>"))
	   (if (equal interim._type "timecard-continual")
	       (+ "<a href=\"/ecm/claim/" interim.claim_id "/interim/create\""
		  " class=\"btn btn-outline-secondary btn-sm\""
		  " style=\"color:black; float:right;\">"
		  " Create Interim</a>")
	       "")
	   "</div></div></td></tr>"))
      )
    '| 
    $(document).ready(function() {

    |(ps:ps
       (defun make-timecard-note-row (note attachment)
	       (when (or note attachment)
	         (+ "<tr><td colspan=\"7\">
           <div class=\"cheque text-xs-center\" style=\"margin:0px 10%;\">"
	            (if note (+"<blockquote style=\"white-space: pre-wrap\">" note "</blockquote>") "")
	            (if attachment
		              (+ "<strong>attachment: </strong>" attachment)
		              "")
	            "</div> </td></tr>")))

       )
    '|
    var interims = $('tr[data-timecard-interim-id]').clone();
    view_interims = [];
    view_all_interims = true;
    collapse_all_interims = false;
    old_timecard_table = $('#| id '|').clone();

    timecard_table = $('#|
    id '|').DataTable({
        "timecard": true,
        "autoWidth": false,
         responsive: {
             details: {
                 display: $.fn.dataTable.Responsive.display.childRowImmediate,
                 type: 'none',
                 target: ''
             }
         },
        "paging" : false,
        "pageLength": 200,
        "columnDefs" :
         [
           { orderable: false, targets: [10] },
           { "visible": false, "targets": [0,7,8,9] }
         ],
        "drawCallback": function ( settings ) {
            var api = this.api();
            var rows = api.rows( {page:'current'} ).nodes();

           var na = api.columns([7,8], {page:'current'}).data();
           var notes = na[0];
           var attach = na[1];

           $(notes).each( function (i, data) { 
             var notea = |(ps:ps (make-timecard-note-row data (aref attach i))) '|;
             $(rows).eq( i ).after( notea );
              var this_row = $(rows)[i] ;
   
              $(this_row).find('td:first').css('border-left', '1px solid lightgray');
              $(this_row).find('td:last').css('border-right', '1px solid lightgray');
              $(this_row).find('td').css({"padding-top": '0.5em', "border-top" : "1px solid lightgray", "border-bottom" : "none"});
             
               $(this_row).next().find('td:first').css('border-left', '1px solid lightgray');
              $(this_row).next().find('td:last').css('border-right', '1px solid lightgray');
              $(this_row).next().find('td').css({"margin-bottom": '10px', "border-top" : "none", "border-bottom" : "1px solid lightgray"});

             });


            // interim
            var last=null;
            api.column(9, {page:'current'} ).data().each( function ( interim_id, i ) {
                if ( last !== interim_id && interim_id != "" && interim_id != 'total') {
//                   alert(interim_id);
                  |(ps:ps (make-interim-border (|.| ($ rows) (eq i)))) '|
                  $(rows).eq( i ).prev().find("td").css("border-bottom", "2px solid lightgrey;margin-bottom:0.5em;");
                  var sel = '[data-timecard-interim-id='+interim_id+']';
                  var interim = $(interims).filter(sel).data('timecard-interim') ;
                   // alert(interim);
                   // console.log(interim);
                  // console.log(api);
                   var searchL = api.search().length;
                  var header = |
    (ps:ps (make-interim-header interim "Interim"
				(or (eql 0 search-l))))
    '|
                  // alert(header);
                 // console.log(interim.claim_id);
                    $(rows).eq( i ).before(
                        header
                    );
                    last = interim_id;
                } else if (last = interim_id) {
                  | (ps:ps (make-interim-border (|.| ($ rows) (eq i)))) '|

                }
                // now for the note and attachment
                 var n_and_a = api.columns([7,8], {page:'current'} ).data();
                 var note = n_and_a[0];
                 var attachment = n_and_a[1];

          });
        }

   // /datatable    
    });

   $('#| id '|').dataTable().fnSettings().timecard = true;
|
    (ps:ps

      (ecm/ps:|.| $ fn data-table ext search
	      (push (lambda (settings data data-index)
		      ;;  (console.log settings.timecard)
		      ;;  (console.log (length (timecard_table.search)))
		      (if (or (not (eq settings.timecard t))
			      (>= (length (timecard_table.search)) 1))
			  t
			  (let ((node (ecm/ps:|.| timecard_table
					      (row data-index)
					      (node))))
				
			    (cond ((ecm/ps:$. node (attr "data-timecard-interim-id"))
				   t)
				  (collapse_all_interims
				   (setf view_interims (array))
				   nil)
				  ((let ((interim
					      (ecm/ps:$. node (attr "data-timecard-in-interim"))))
					 (or (member interim view_interims)
					     (when view_all_interims
					       (ecm/ps:|.| view_interims (push interim))
					       t)))
				       t)
				  (t ps:false)))))))
      (|.| timecard_table
	   (on "draw"
	       (lambda ()
		 (ecm/ps:$.
		  ".hover-timecard" 
		  (click (lambda ()
			   (let* ((interim (ecm/ps:$. this (attr "data-interim-id")))
				  (down 
				   (if (member interim view_interims)
				       t
				       ps:false)))
			     
			     (if down
				 (remove interim view_interims)
				 (ecm/ps:|.| view_interims (push interim)))
					;			     (console.log interim)
					;			     (console.log this)
			     (|.| timecard_table (draw))))))
		 (setf view_all_interims t)
		 (setf collapse_all_interims ps:false))))

      (|.| timecard_table (draw)))

#+(or)    '|
$("#ecmClaimTimecardTable").dataTable().parent().parent().parent().find("#ecmClaimTimecardTable_length").append('<a href="#" id="ecmTimecardShowInterims"> Expand All Interims</a> / <a href="#" id="ecmTimecardHideInterims"> Collapse All Interims</a>');

 |

    		 (ps:ps (ecm/ps:$.
		  "#ecmTimecardShowInterims" 
		  (click (lambda (e)
			   (|.| e (prevent-default))
			   (setf view_all_interims t)
			   (|.| timecard_table (draw))))))
		 (ps:ps (ecm/ps:$.
			 "#ecmTimecardHideInterims" 
			 (click (lambda (e)
				  (|.| e (prevent-default))
				  (setf collapse_all_interims t)
				  (|.| timecard_table (draw))))))
		 '|
 // /ready

  });

$(document).ready(function() { 

  $("#ecmTimecardShowInterims").trigger("click");

  $("#ecmClaimTimecardTable_filter").prepend($("#ecmClaimTimecardTable #timecardCreateNew").first().clone());



});
|))

(defun <claim-timecard-table> (claim-timecards
			       &rest args
			       &key
				 (claim-id t)
				 (buttons t)
				 (interim-id t)
				 (id "ecmClaimTimecardTable"))
  (setf claim-timecards
	(remove-if (lambda (tc)
		     (and (member (getjso "_type" tc)
				  '("timecard-continual"
				    "timecard-total")
				  :test #'string-equal)
			  (every (lambda (key)
				   (member (getjso key tc) '(:null nil)))
				 '("billable_hours"
				   "unbillable_hours"
				   "mileage"
				   "disbursement"))))
		   claim-timecards))
  (<timecard-style> :id id)
  (<> (table :class "table table-sm compact persist-area"
	     :id id
	     :style "background-color:white"
	     :cellspacing "0" :width "100%"
	     :data-persistent-class "floatingHeader floatingTr"
	     :data-persistent-header ".persist-header")
    (<> (thead :class "thead-dark")
      (<> (tr :class "persist-header")
	(when claim-id
	  (<> 'th (<> :unescaped "Claim&nbsp;#")))
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "User")
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Date")

	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Billable Hours")
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Unbillable Hours")
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Mileage")
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Disbursement")
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Notes")
	(<> (th :class "text-xs-center"
		:style "padding-right: 0px;")
	  "Attachment")
	(when interim-id
	  (<> (th :class "text-xs-center"
		  :style "padding-right: 0px;")
	    "Interim ID"))
	(when buttons
	  (<> (th :class "text-center")
	    
	    (<> '(html5:div :class "center-block timecard-buttons align-middle"
		  :style "text-align:center; padding-top:2.5px;;")
	      (<> '(html5:div 
		    :class "btn-group text-xs-center")
		(<> `(html5:a :class "btn btn-light btn-outline-secondary btn-sm"
                 :id "timecardCreateNew" 
			      :href ,(concatenate
				      'string
				      "/ecm/create?create[type]=timecard&create[key]=claim-id&access[type]=claim"
				      "&access[id]=" (princ-to-string claim-id)
				      "&back[active-tab]=Timecard"))
		  (<> "New"))))))))
    (<> 'tbody
      (dolist (tr claim-timecards)
	(apply #'<claim-timecard-tr> tr args))))
  (<timecard-script> :id id)
					; (<timecard-edit-script>)
  )

(defun <claim-timecard-tr> (timecard
			    &key 
			      (claim-id t)
			      (buttons t)
			      (interim-id t)
			      &allow-other-keys)
  (let* ((claim-id (when claim-id (getjso "claim_id" timecard)))
	     (_id (getjso "_id" timecard))
	     (_type (getjso "_type" timecard))
	     (date (getjso "date" timecard))
	     (effective-time (getjso "effective_time" timecard))
	     (expiry-time (getjso "expiry_time" timecard))
	     (user (getjso "user" timecard))
	     (billable-hours (getjso "billable_hours" timecard))
	     (unbillable-hours (getjso "unbillable_hours" timecard))
	     (mileage (getjso "mileage" timecard))
	     (disbursement (getjso "disbursement" timecard))
	     (notes (getjso "notes" timecard))
	     (attachment (getjso "attachment" timecard))
		 (invoice-id (getjso "invoice_id" timecard))
		 
	     (interim-id (when interim-id
		               (cond ((string-equal _type "timecard-interim")
			                  _id)
			                 ((string-equal _type "timecard-continual")
			                  "continual")
			                 ((string-equal _type "timecard-total")
			                  "total")
			                 (t 
			                  (or (getjso "interim_id" timecard)
				                  "continual"))))))
                                        ;    (break "~A : ~A " _type interim-id)
    (<> (tr (cond ((string= _type "claim-timecard")
		           (list :data-timecard-id _id
			             :data-timecard-in-interim interim-id))
		          ((or (string= _type "timecard-interim")
		               (string-equal _type "timecard-continual")
		               (string-equal _type "timecard-total"))
		           ;;		   (break "~A" _type)
		           (list :data-timecard-interim-id interim-id
			             :data-timecard-interim (ecm/json:write-json-to-string
						                         timecard)))))

      (when claim-id
	    (<> (td :class "text-xs-center")
	      (<> :text (getjso "claim_id" timecard))))
	  (<> (td :class "text-xs-center")
	    (cond (user
		       (<> :text (ecm/entity/corpus:corpus-name-as-string
				          (getjso "corpus" user)
				          :company-name nil
				          :province nil)))
		      ((string= _type "timecard-interim")
		       (<> (div :class "alert alert-info")
		         (<> "Interim")
	
		         (<> (small :class "text-muted")
			       (<> :text (ecm/ui/utility:format-timestring
							  effective-time :format +timecard-date-format+))
				   (when invoice-id 
					 (<> (button :type "button" :class "btn btn-outline-secondary btn-sm fa fa-book" :data-toggle "tooltip" :data-placement "top" :title (format nil "View Invoice #~A" invoice-id)))))))
		       ((string= _type "timecard-continual")
				(<> (div :class "alert alert-warning")
		          (<> "Continual")
		          (<> (small :class "text-muted")
					(<> :text " from " (ecm/ui/utility:format-timestring
					                   effective-time)))))
		      ((string= _type "timecard-total")
		       (<> (div :class "alert bg-inverse text-white")
		         (<> "Total")))))
      
	  (<> (td :class "text-xs-center")
	    (when date (<> :text            ;date
			         (ecm/ui/utility:format-timestring
                      date :format +timecard-date-format+)))
	    (when expiry-time (<> :text (ecm/ui/utility:format-timestring expiry-time))))
       

	  (<> (td :class "text-xs-center")
	    (<> :text billable-hours))

	  (<> (td :class "text-xs-center")
	    (<> :text unbillable-hours))

	  (<> (td :class "text-xs-center")
	    (<> :text mileage))
      
	  (<> (td :class "text-xs-center"
		      :style "padding-left:0.5rem;padding-right:0.5rem")
	    (<> :text "$" disbursement))

	  (<> (td :class "text-xs-center")
	    (when notes (<> :text notes)))

	  (<> (td :class "text-xs-center")
	    (when attachment
		  (ecm/ui/attachment:<link-to-attachment> attachment)))

	  (<> (td :class "text-xs-center"
		      :data-interim-id interim-id)
	    (cond (interim-id
		       (<> :text interim-id))))
      
	  (when buttons
		(<> (td :class "text-xs-center")
		  (cond ((string= _type "claim-timecard")
			     (<timecard-buttons> _id claim-id))
			    ((string= _type "timecard-continual")
			     (<continual-buttons> claim-id))
			    ((string= _type "timecard-interim")
			     (<interim-buttons> _id claim-id))
			    (t #+(or)(<> :text _type))))))))
				 
(defun <timecard-buttons> (_id claim-id)
  (<> '(html5:div :class "center-block timecard-buttons")
    (<> '(html5:div :class "btn-group text-xs-center"
	        :style "width: 70%; ")
      (<> (html5:button :type "button"
                        :class "btn btn-secondary btn-sm dropdown-toggle"
                        :data-toggle "dropdown")
        (<> (i :class "fa fa-bars") " "))
      (<> '(html5:div :class "dropdown-menu")
        (<> `(html5:a :class "dropdown-item"
		                  :href ,(concatenate
			                        'string
			                        "/ecm/view?timecard="
			                        (princ-to-string _id)
			                        "&go-back-to-claim="(princ-to-string claim-id))
		                  :target "_blank")
	        (<> "View"))
        (when (or (ecm/user:user-is-administrator-p)
		              (ecm/entity/timecard:user-can-edit-timecard-p
		               (ecm/user:user) _id))
	        (<> `(html5:a :class "dropdown-item"
		                    :href ,(concatenate
			                          'string
			                          "/ecm/edit?timecard="
			                          (princ-to-string _id)
			                          "&go-back-to-claim="
			                          (princ-to-string claim-id)
			                          "&access[type]=claim&access[id]="
			                          (princ-to-string claim-id)))
	          (<> "Edit")))
        (when (ecm/user:user-is-administrator-p)
	        (<> `(html5:a :class "dropdown-item"
		                    :href ,(concatenate
			                          'string
			                          "/ecm/delete?timecard="
			                          (princ-to-string _id)			    
			                          "&go-back-to-claim="(princ-to-string claim-id)
			                          "&access[type]=claim&access[id]="
			                          (princ-to-string claim-id)))
	          (<> "Delete")))))))

(defun <interim-buttons> (_id claim-id)
      
  (<> '(html5:div :class "center-block timecard-buttons")
    (<> '(html5:div :class "btn-group text-center"
	        :style "width: 70%; ")
      (<> (html5:button :type "button"
                        :class "btn btn-secondary btn-sm dropdown-toggle"
                        :data-toggle "dropdown")
        (<> (i :class "fa fa-bars") " "))
      
      (<> '(html5:div :class "dropdown-menu")
      (<> (html5:a :class "btn btn-secondary btn-sm"
		           :href (cat
                          "/report/interim/"
			    (princ-to-string _id))
		    :target "_blank")
	(<> "View"))
      (when (ecm/user:user-is-administrator-p)		     
	(<> `(html5:a :class "btn btn-secondary btn-sm"
		      :href ,(concatenate
				    'string
				    "/ecm/interim/"
				    (princ-to-string _id)
				    "/edit"))
	  (<> "Edit"))
	(<> `(html5:a :class "btn btn-secondary btn-sm"
		      :href ,(concatenate
			      'string
			      "/ecm/delete?timecard-interim="
			      (princ-to-string _id)			    
			      "&go-back-to-claim="(princ-to-string claim-id)
			      "&access[type]=claim&access[id]="
			      (princ-to-string claim-id)))
	  (<> "Delete")))))))

(defun <continual-buttons> (claim-id)
      (<> 'style
	" .timecard-buttons a { width: 100%; margin-bottom:5px;} ")
  (<> '(html5:div :class "center-block timecard-buttons"
	:style "text-align:center; padding-top:2.5px; width: 100%;")
    (<> '(html5:div :class "btn-group text-xs-center"
	  :style "width: 70%; ")
      (<> (html5:a :class "btn btn-secondary btn-sm"
		   :href (cat "/ecm/claim/" claim-id "/interim/create")
		   :style "background-color:black;color:white")
	(<> "Create Interim")))))

(defun <timecard-navbar> (claim-id
			  insured
			  title
			  timecard-id
			  status)
  (declare (ignore title))
  (<> (ecm/ui/navbar:navbar :type "timecard" :id timecard-id)
        (multiple-value-bind (c i s)
	    (ecm/ui/utility:values-claim/insured/status claim-id insured status)
	  (<> 'html5:style
	" html, body {
    height: 100%;
 
}
body #navClaimTitle {
 text-align:center;
 text-overflow : ellipsis;
 white-space   : nowrap;
 padding-top: 0.5rem;
 
}

.ecm-nav .dropdown-menu {
  top: 2.75rem; left: 0.25em;
}
")
      
      (<> (div :id "navClaimTitle")
	(<> :unescaped "Timecard&nbsp;for&nbsp;")
	(<> (html5:a :href (concatenate
			      'string "/ecm/claim/" (princ-to-string claim-id))
		     :style "color : white")
	  (funcall c))
	(<> :unescaped "&nbsp;")
	(funcall s)
	(<> :unescaped "&nbsp;")
	(funcall i)))))

(defun <create-timecard-navbar> (claim-id
				 insured)
  
    (<> '(ecm/ui/navbar:navbar)
      (<> '(div :class "center-block" :style "display:inline-block;position:relative;")
	(<> '(html5:h1 :style "display: inline-block")
	  (<> :text "New Timecard")
	  (<> '(html5:small :class "text-muted") " for Claim #")
	  (<> (html5:a :href (concatenate
			      'string "/ecm/claim/" (princ-to-string claim-id))
		       :style "color : white")
	    (<> :unescaped claim-id "&nbsp;")))
	  
	  (<> '(html5:h4 :style "display: inline-block")
	    (<> '(html5:small :class "text-muted") "insured")
	    (<> :unescaped "&nbsp;")
	    (<> `(html5:a :href ,(concatenate
				  'string "/ecm/view?person="
				  (princ-to-string (getjso "_id" insured)))
			  :style "color : white"
			  :target "_blank")
	      (<> :text (corpus-name-as-string insured)))))))


(defmacro <create-timecard-form> (&body body)
  `(<> '(html5:form
	 :method "POST")
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
		   "pickerTimeFormat" "hh:mm tt Z"		   
		   "timeInput" t
		   "dateFormat" "D, d M yy"
		   "maxDate"  0
		   "controlType" "select"
		   "oneLine" t)))))))
     ,@body))



(defun <item> (label data)
  (when data
    (<> '(html5:small :class "text-muted")
	    (<> :unescaped label "&nbsp;"))
    (<> :text data) (<> :unescaped "&nbsp;")))


(defun <attachment-radio> (claim-id
			   &key
			     (allow-none nil)
			     (checked-attachment-id nil))
  (let* ((attachments (postmodern:query
		       (:select (:jsi.claim-attachments claim-id))
		       :single))
	       (attachments (ecm/json:read-json-from-string attachments))
	       (attachments (ecm/json:null->nil
		                   (ecm/json:getjso "attachments" attachments)))
         ;; Sort by date desc
         (attachments (sort attachments #'string>
                            :key (lambda (a)
                                   (getjso "time" a)))))

;;; For scrolling, find the offset of the first and subtract it from the offset
;;; of the selected for the scrolltop
    (<> (html5:script)
      "$(document).ready(function() {
   var first_offset = $('#ecmAttachmentRadio :input').first().offset().top;
 $('#ecmAttachmentRadio :input').each(function() {
        var radioInput = $(this);
        if(radioInput.is(':checked')) {

         var scroll = radioInput.offset().top - first_offset;
            $('#ecmAttachmentRadio').animate({
                scrollTop: (scroll)
            }, 500);
        }

    });
});")
    (<> 'html5:style
	   "

.ecm-check-label {
  margin-left: 0.5rem;
  padding:0.5rem;
}

label .ecm-check {
display:none;
}

label.active .ecm-check {
display:inline;
}
label.active i {
  display : none ;
}

label.active {
  background-color: #d4edda;
}
.btn-light:not(:disabled):not(.disabled).active {

  background-color: #d4edda;
}

.active .alert-info {
  background-color: #d4edda;
}



       ")
    ;; This is the wrapper for the list of "radio" Buttons

    (<> '(html5:div :class "ecm-radio btn-group btn-group-toggle"
          :data-toggle "buttons"
          :style "display:block;"
	        :id "ecmAttachmentRadio")

      

      (labels ((check-button (value label checked?)
                 (let ((class (concatenate 'string
                                           "btn btn-light"
                                           (if checked? " active" ""))))
                   (<> (div :class "row btn-group-toggle"
                            :style "border-bottom: 1px solid grey")
                     (<> (div :class "col-1 btn-group-toggle")
                     (<> (html5:label :class class
                                      :style "
float:left;
border:0;
margin:0;
white-space:normal;
display: inline-block;")
                       (<> (html5:input
                            :type "radio" :name "ecmAttachment"
                            :value value
                            :id value
                            :autocomplete "off"
		                        (when checked? "checked"))
                         )
                       (<> (i :class "alert alert-info fa fa-square-o"
                              :style "padding:5px;")
	                       (<> (span :class "ecm-radio-text")
		                       (<> :text " ")))

                       (<> (i :class "ecm-check alert alert-success fa fa-check-square-o"
                              :style "padding:5px;")
	                       (<> (span)
		                       (<> :text " ")))))

                     (<> (div :class "col")
                       

                       (typecase label
                         (string (<> :text label))
                         (t (let ((a label))
                              (<> (html5:div :class "ecm-check-label")
                                (<> :text (ecm/json:null->nil (getjso "description" a))
                                    " ")
	                              (<item> "file name:" (getjso "file_name" a))
	                              (<item> "date:" (getjso "date" a)))))))))))

        (when allow-none
          (check-button "" "None" nil))
        (dolist (a attachments)
          (let ((checked? (equal (getjso "_id" a) checked-attachment-id)))
            (check-button (getjso "_id" a) a checked?)))))))

(defun <user-select> (active-user-id)
  (let* ((users 			(postmodern:query "SELECT jsi.user_summary(app_user_id) FROM (SELECT DISTINCT app_user_id, count(*) AS number FROM timecard GROUP BY app_user_id ORDER BY number DESC) AS users;"
							  :column))
;	 (foo (break "~A" users))
	 (users (mapcar #'ecm/json:read-json-from-string
			(remove :null users))))
    (<> (html5:select :class "form-control")
      (dolist (u users)
	(let ((uid (getjso "_id" u)))
	  (<> (html5:option :class "form-control"
			    :value uid
			    (when (equal active-user-id uid)
			      "selected"))
	    (<> :text (let* ((corpus (ecm/json:getjso* "corpus" u))
			     (name (ecm/entity/corpus:corpus-name-as-string
				    corpus)))
			(if (and corpus (not (string-equal name nil)))
			    name
			    (getjso "username" u))))))))))
(defmacro <timecard-form> ((&key edit) &body body)
  `(,(if edit
       '<create-timecard-form> 
       'progn)
     ,@body))
       
(defun timecard-page (&key claim-id timecard-id

			(title "Update Timecard")
			(navbar-title title)
			(error nil)
			(edit t)
			(submit-text "Submit Update")
		
			user-id		
			date
			notes
			billable-hours
			unbillable-hours
			mileage
			disbursement
			attachment-id &allow-other-keys)
  (<> (ecm/ui/page:page
       :title title)
    (let* ((claim-summary
	    (postmodern:query
	     (:select (:jsi.claim-summary claim-id))
	     :single))
	   (claim
	    (ecm/json:read-json-from-string
	     claim-summary))		;
	   (risk (getjso "risk" claim))
	   (status (getjso "status" claim))
	   (policy (getjso "policy" risk))
	   (insured (getjso "insured" policy))
	   (user (when user-id
		   (ecm/entity/user:find-user user-id))))
      (<timecard-navbar> claim-id insured navbar-title timecard-id status)
      (<> 'html5:style
	"
#ecmBody {
  padding-top: 1.5rem;
}

.form-group label { font-weight: bold; }"
      	".form-group .form-check-label { font-weight: normal; }")
      
      (<timecard-form> (:edit edit)
	(when edit
	  (<> (html5:input :type "hidden" :name "claim-id" :value claim-id))
	  (<> (html5:input :type "hidden" :name "user-id" :value user-id))
	  (when timecard-id
	    (<> (html5:input
		 :type "hidden"
		 :name "timecard-id"
		 :value timecard-id))))
	(<> '(html5:div :class "row")	  
	  (when (and error (not (string= "" error)))
					;(break "~A" error)
	    (<> '(html5:code)
	      (<> :text error)
	      (<> 'hr)))
	  (<> 'br)
	  (<> '(html5:div :class "col-md-4")
	    (<> '(html5:fieldset :class "form-group")
	      (<> '(html5:label :for "ecmTimecardDatepicker")
		(<> "Date & Time"))
	      (if edit 
		  (<> (html5:input
		       :class "form-control datepicker"
		       :id "ecmTimecardDatepicker"
		       :name "date"
		       :type "text"
		       :style "position: relative; z-index: 100000;"
		       :value (if date
				  (ecm/ui/utility:format-timestring date)
				  (ecm/ui/utility:format-timestring
				   (ecm/local-time:format-rfc3339-timestring
				    t (ecm/local-time:universal-to-timestamp
				       (get-universal-time))))))
		    )
		  (<> (html5:p :class "form-control-static" )
		    (when date (<> :text 
				   (ecm/ui/utility:format-timestring date))))))

	    (<> '(html5:table :class "table table-sm form-group form-inline")
	      (<> '(html5:tr)
		(<> '(html5:th)
		  (<> '(html5:label :for "ecmBillable")
		    (<> "Billable Hours")))
		(<> '(html5:td :style "text-align: center")
		  (if edit 
		      (<> `(html5:input
			    :class "ecm-input-hours form-control"
			    :id "ecmBillable"
			    :name "billable"
			    :size 4
			    :type "text"
			    :placeholder "00.00"
			    ,@(when billable-hours (list :value billable-hours))))
		  
		      (<> (html5:p :class "form-control-static" )
			(<> :text billable-hours)))))

	    
	      (<> '(html5:tr)
		(<> '(html5:th)
		  (<> '(html5:label :for "ecmUnbillable")
		    (<> :unescaped "Unbillable" "&nbsp;" "Hours" "&nbsp;")))
		(<> '(html5:td :style "text-align: center")
		  (if edit 
		      (<> `(html5:input
			    :class "ecm-input-hours form-control"
			    :id "ecmUnbillable"
			    :name "unbillable"
			    :type "text"
			    :size 4
			    :placeholder "00.00"
			    ,@(when unbillable-hours (list :value unbillable-hours))))
		      (<> (html5:p :class "form-control-static" )
			(<> :text unbillable-hours)))))

	      (<> '(html5:tr)
		(<> '(html5:th)
		  (<> '(html5:label :for "ecmMileage")
		    (<> "Mileage")))
		(<> '(html5:td :style "text-align: center")
		  (if edit 
		      (<> (html5:input
			   :class "ecm-input-hours form-control"
			   :id "ecmMileage"
			   :type "text"
			   :name "mileage"
			   :size 4
			   :placeholder "00.00"
			   (when mileage (list :value mileage))))
		      (<> (html5:p :class "form-control-static" )
			(<> :text mileage)))))

	      (<> '(html5:tr)
		(<> '(html5:th)
		  (<> '(html5:label :for "ecmDisbursement")
		    (<> "Disbursement")))
		(<> '(html5:td :style "text-align: center")
		  (<> '(html5:div :class "input-group")
		    (when edit
		      (<> '(html5:div :class "input-group-addon")
			"$"))
		    (if edit 
			(<> `(html5:input
			      :class "ecm-input-hours form-control"
			      :id "ecmDisbursement"
			      :name "disbursement"
			      :type "text"
			      :size 4
			      :placeholder "00.00"
			      ,@(when disbursement (list :value disbursement))))
			(<> (html5:p :class "form-control-static" )
			  (<> :text "$"disbursement))))))))

	    
	  (<> '(html5:div :class "col-md-8")
	    (<> '(html5:fieldset :class "form-group")
	      (<> '(html5:label :for "ecmUser")
		(<> "User"))
	      (when user (if edit 
			     (<user-select> user-id)
			     (<> :text " " (corpus-name-as-string (getjso "corpus" user))))))
	    (<> '(html5:fieldset :class "form-group")
	      (<> '(html5:label :for "ecmNotesTextarea")
		(<> "Notes"))
	      (if edit 
		  (<> '(html5:textarea :class "form-control" :id "ecmNotesTextarea"
			:name "notes"
			:rows "6")
		    (<> :text (or notes "")))
		  (<> (html5:blockquote :class "blockquote")
		    (<> :text (or notes "")))))))
	(when (or edit attachment-id)
	  (<> '(html5:div :class "row")
	    (<> '(html5:fieldset :class "form-group form-inline")

	      (<> '(html5:div :class "col-md-2")
		(<> '(html5:label :for "ecmAttachment"
		      :style "display:inline")
		  (<> "Attachment")))

	      (if edit
		  (<> '(html5:div :class "col-md-10")
		    (<attachment-radio> claim-id :allow-none t :checked-attachment-id attachment-id))
		  (when attachment-id
		    (let ((a (ecm/entity/attachment:find-attachment attachment-id)))
		      (when a
			(ecm/ui/attachment:<link-to-attachment> a))))))))
      
	(when edit (<> '(html5:div :class "row")
		     (<> '(html5:div :class "col-md-12"
			   :style "text-align: center")
		       (<> '(html5:button :type "submit"
			     :class "btn btn-success"
			     :style "float:left")
			 (<> :text submit-text))
		       (<> `(html5:a :href ,(concatenate
					     'string "/ecm/claim/"
					     (princ-to-string claim-id))
				     :class "btn btn-danger"
				     :style "float:right")
			 (<> "Cancel")))))))))
  

(defun create-timecard-page (claim-id
			     &key
			       (error nil)
			   ;    user-id
			       date
			  notes
			  billable-hours
			  unbillable-hours
			  mileage
			  disbursement
			       ;attachment-id
			       &allow-other-keys)
  (<> '(ecm/ui/page:page
	:title "Create Timecard")
    (let* (                ; (claim-id (ignore-errors (parse-integer claim-id)))
	         (claim-summary
	           (postmodern:query
	            (:select (:jsi.claim-summary claim-id))
	            :single))
	         (claim
	           (ecm/json:read-json-from-string
	            claim-summary))
                                        ;
	         (risk (getjso "risk" claim))
	         (policy (getjso "policy" risk))
	         (insured (getjso "insured" policy)))
      (<timecard-navbar> claim-id insured "New Timecard" nil (getjso "status" claim))
      (<> 'html5:style
	".form-group label { font-weight: bold; }"
      	".form-group .form-check-label { font-weight: normal; }")
      
      (<create-timecard-form>
        (<> (div :class "pt-4")
	      (<> '(html5:div :class "row ")
	        (when error 
	          (<> '(html5:code)
	            (<> :text error)
	            (<> 'hr)))
	  
	        (<> '(html5:div :class "col-md-4")
	          (<> '(html5:fieldset :class "form-group")
	            (<> '(html5:label :for "ecmTimecardDatepicker")
		            (<> "Date & Time"))
	            (<> (html5:input
		               :class "form-control datepicker"
		               :id "ecmTimecardDatepicker"
		               :name "date"
		               :type "text"
		               :style "position: relative; z-index: 100000;"
		               :value (if date
			                        (ecm/ui/utility:format-timestring date)
			                        (ecm/ui/utility:format-timestring
			                         (ecm/local-time:format-rfc3339-timestring
				                        t (ecm/local-time:universal-to-timestamp
				                           (get-universal-time))))))))

	          (<> '(html5:table :class "table table-sm form-group form-inline")
	            (<> '(html5:tr)
		            (<> '(html5:th)
		              (<> '(html5:label :for "ecmBillable")
		                (<> "Billable Hours")))
		            (<> '(html5:td :style "text-align: center")
		              (<> `(html5:input
			                  :class "ecm-input-hours form-control"
			                  :id "ecmBillable"
			                  :name "billable"
			                  :size 4
			                  :type "text"
			                  :placeholder "00.00"
			                  ,@(when billable-hours (list :value billable-hours))
			                  ))))

	    
	            (<> '(html5:tr)
		            (<> '(html5:th)
		              (<> '(html5:label :for "ecmUnbillable")
		                (<> :unescaped "Unbillable" "&nbsp;" "Hours" "&nbsp;")))
		            (<> '(html5:td :style "text-align: center")
		              (<> `(html5:input
			                  :class "ecm-input-hours form-control"
			                  :id "ecmUnbillable"
			                  :name "unbillable"
			                  :type "text"
			                  :size 4
			                  :placeholder "00.00"
			                  ,@(when unbillable-hours (list :value unbillable-hours))))))

	            (<> '(html5:tr)
		            (<> '(html5:th)
		              (<> '(html5:label :for "ecmMileage")
		                (<> "Mileage")))
		            (<> '(html5:td :style "text-align: center")
		              (<> `(html5:input
			                  :class "ecm-input-hours form-control"
			                  :id "ecmMileage"
			                  :type "text"
			                  :name "mileage"
			                  :size 4
			                  :placeholder "00.00"
			                  ,@(when mileage (list :value mileage))))))

	            (<> '(html5:tr)
		            (<> '(html5:th)
		              (<> '(html5:label :for "ecmDisbursement")
		                (<> "Disbursement")))
		            (<> '(html5:td :style "text-align: center")
		              (<> '(html5:div :class "input-group")
		                (<> '(html5:div :class "input-group-addon")
		                  "$")
		                (<> `(html5:input
			                    :class "ecm-input-hours form-control"
			                    :id "ecmDisbursement"
			                    :name "disbursement"
			                    :type "text"
			                    :size 4
			                    :placeholder "00.00"
			                    ,@(when disbursement (list :value disbursement)))))))))

	    
	        (<> '(html5:div :class "col-md-8")
	          (<> '(html5:fieldset :class "form-group")
	            (<> '(html5:label :for "ecmNotesTextarea")
		            (<> "Notes"))
	            (<> '(html5:textarea :class "form-control" :id "ecmNotesTextarea"
		                :name "notes"
		                :rows "9")
		            (<> :text (or notes ""))))))

	      (<> '(html5:div :class "row" :style "max-height:40vh; overflow-y: scroll")
          (<> '(html5:div :class "col-md-2")
	          (<> '(html5:label :for "ecmAttachment"
		              :style "display:inline")
		          (<> "Attachment"))) 

	    
	        (<> '(html5:div :class "col-md-10")
	          (<attachment-radio> claim-id))
	        (<> '(html5:fieldset :class "form-group form-inline")
	    
	          ))

	      (<> '(html5:div :class "row")
	        (<> '(html5:div :class "col-md-12"
		            :style "text-align: center")
	          (<> '(html5:button :type "submit"
		              :class "btn btn-success"
		              :style "float:left")
	            (<> "Submit New Timecard"))
	          (<> `(html5:a :href ,(concatenate
				                          'string "/ecm/claim/"
				                          (princ-to-string claim-id))
			                    :class "btn btn-danger"
			                    :style "float:right")
	            (<> "Cancel"))))))

      #+(or)(<> '(html5:div :class "container-fluid")
	      (<> '(html5:div :class "row")
		(<> `(html5:div :class "col-md-8")
		  (<> :unescaped '|<div id="summernote">Hello Summernote</div>|)
		  (<> 'html5:script "
$(document).ready(function() {
  $('#summernote').summernote();
});"))

		)))))

(defmacro <interim-form> (&body body)
  `(<create-timecard-form> ,@body))

(defun create-or-edit-interim-page (&key
				      (claim-id)
				      (interim-id)
				      (edit t)
				      (create t)
				      (date nil)
				      (error nil))
  (let* ((interim
	  (when interim-id
	    (ecm/entity/timecard:find-interim interim-id)))
	 (continual
	  (or interim
	      (ecm/entity/timecard:find-claim-continual-timecard
	       claim-id)))
	 (claim (if interim
		    (getjso "claim" interim)
		    (ecm/entity/claim:claim-summary claim-id)))
	 (insured (ecm/json:getjso* "risk.policy.insured" claim))
	 (date (or date (getjso "expiry_time" continual))))

    (when (not claim-id)
      (setf claim-id (getjso "_id" claim)))
    (<> (ecm/ui/page:page
	 :title (cat (if create "Create" "Edit")" Interim"))
      (<timecard-navbar> claim-id insured "Interim" nil
			 (getjso "status" claim))
      (<interim-form>
	(<> (div :class "text-xs-center align-middle"
		 :style "margin:auto;")
	  (<> (hr))
	  (<> (h1)
	    (<> :text (cat (if create "Create" "Edit")" Interim"))
	    (<> (small :class "text-muted")
	    " from ")
	    (<> :text (ecm/ui/utility:format-timestring
		       (getjso "effective_time" continual))))
	  (<> (h3)
	    (<> '(html5:label :for "ecmTimecardDatepicker"
		  :class "text-muted")
	      (<> "to")))
	  (if edit 
	      (<> `(html5:input
		    :class "form-control datepicker text-xs-center"
		    :id "ecmTimecardDatepicker"
		    :name "date"
		    :type "text"
		    :style "width:32rem; font-size:195%; margin:auto; position:relative; z-index: 50000"
		    ,@(when date
			(list :value
			      (ecm/ui/utility:format-timestring date)))))
	      (<> (html5:p :class "form-control-static" )
		(when date
		  (<> :text (ecm/ui/utility:format-timestring date)))))
	  (when error
	    (<> (code) (<> (pre) (<> :text (format nil "ERROR: ~A" error)))))
	  (<> '(html5:fieldset :class "form-group text-xs-center")
	    (<> '(html5:div :class "row")
	      (<> '(html5:div :class "col-md-12"
		    :style "text-align: center")
		(<> (hr))
		(<> '(html5:button :type "submit"
		      :class "btn btn-success"
		      :style "float:left")
		  (<> :text (if create "Create New" "Edit") " Interim"))
		(<> `(html5:a :href ,(concatenate
				      'string "/ecm/claim/"
				      (princ-to-string claim-id))
			      :class "btn btn-danger"
			      :style "float:right")
		  (<> "Cancel"))))))))))

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


(defun <interim-navbar> (claim-id
			 insured			
			 interim-id
			 status)
  
  (<> (ecm/ui/navbar:navbar :type "timecard-interim" :id interim-id)
        (multiple-value-bind (c i s)
	    (ecm/ui/utility:values-claim/insured/status claim-id insured status)
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
 margin:0;
 padding:0;
 top: 1rem; right:6rem; left:5.5em;
 
}

.ecm-nav .dropdown-menu {
  top: 2.75rem; left: 0.25em;
}
")
      
      (<> (div :id "navClaimTitle")
	(<> :unescaped "Interim&nbsp;for&nbsp;")
	(<> (html5:a :href (concatenate
			      'string "/ecm/claim/" (princ-to-string claim-id))
		     :style "color : white")
	  (funcall c))
	(<> :unescaped "&nbsp;")
	(funcall s)
	(<> :unescaped "&nbsp;")
	(funcall i)))))

(defun interim-page (interim-id)
  (let* ((interim (ecm/entity/timecard:find-interim interim-id))
	 (claim (getjso "claim" interim))
	 (claim-id (getjso "_id" claim)))
    (<> (ecm/ui/page:page :title "Timecard Interim")
      (<interim-navbar> claim-id
			(ecm/json:getjso* "risk.policy.insured" claim)
			interim-id
			(getjso "status" claim))
      (<> (div :class "row text-xs-center")
	(<claim-timecard-table>
	 (append (getjso "timecards" interim)
		 (prog1 (list interim)
		   (setf (getjso "_type" interim)
			 "timecard-total")))
	 :claim-id nil
	 :buttons nil
	 :interim-id nil))

      (go-back-to-claim claim-id))))
	
