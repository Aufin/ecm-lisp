(defpackage :ecm/ui/attachment
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.| #:$.)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/ssconvert)
  (:import-from :drakma)
  (:import-from :ecm/ui/utility
		#:cat)
  (:import-from :ecm/user)
  (:import-from :ecm/json
		#:getjso
		#:getjso*)
  (:import-from :ecm/ui/navbar)
  (:import-from :ecm/endpoint)
  (:import-from :ecm/entity/user)
  (:import-from :ecm/entity/claim)
  (:import-from :ecm/entity/attachment)
  (:import-from :ecm/entity/corpus
		#:corpus-name-as-string)
  (:export #:<link-to-attachment>
	   #:<attachment-table>
	   #:edit-attachment-page
	   #:open-attachments-page))

(in-package :ecm/ui/attachment)

(defun <attachment-style> ()
  (<> 'style
    
  ".dtr-title #claimCreateAttachment { display:none;}

.dtr-data .attachment-buttons {
  display:inline;
  position: absolute;
  top: -2.25rem;
  left: 1rem;
}

  "
  ))
(defmacro <attachment-form> ((&key
			      attachment-id
			      claim-id)
			     &body body)
  
  `(<> (form :method "POST"
	     :action ""
	     :id "attachmentForm")
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
		   "oneLine" t)))))))
     (when ,attachment-id
       (<> (input :type "hidden" :name "attachment-id"
		  :value ,attachment-id)))
     (when ,claim-id
       (<> (input :type "hidden" :name "claim-id"
		  :value ,claim-id)))

     ,@body))

(defun <attachment-navbar> (claim-id			    
			    attachment-id)
  (let ((claim (ecm/entity/claim:find-claim claim-id)))
  (<> (ecm/ui/navbar:navbar :type "attachment" :id attachment-id)
        (multiple-value-bind (c i s)
	    (ecm/ui/utility:values-claim/insured/status
	     claim-id
	     (getjso*  "risk.policy.insured" claim)
	     (getjso "status" claim))
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
 top: 1rem; right:6rem; left:5.5em;
 
}

.ecm-nav { padding: 1em; }
.ecm-nav .dropdown-menu {
  top: 2.75rem; left: 0.25em;
}
")
      
      (<> (div :id "navClaimTitle")
	(<> :unescaped "Attachment&nbsp;for&nbsp;")
	(<> (html5:a :href (concatenate
			      'string "/ecm/claim/" (princ-to-string claim-id))
		     :style "color : white")
	  (funcall c))
	(<> :unescaped "&nbsp;")
	(funcall s)
	(<> :unescaped "&nbsp;")
	(funcall i))))))

(defun edit-attachment-page (&key
			       attachment-id
			       claim-id
			       date
			       file
			       description
			       (title "Edit Attachment")
			       error)
  (let ((attachment-gensym (gensym)))
    (<> (ecm/ui/page:page
	 :title title
	 :dropzone t)
      (<> (div :class "text-center align-middle")
	(<attachment-navbar> claim-id attachment-id)
	(<> 'h1 (<> :text title))
	(when error
	  (<> '(html5:code)
	    (<> :text error)
	    (<> 'hr)))
	(<> (form :class "dropzone"
		  :action "/ecm/attachment/upload"
		  :id "ecmFileDropzone")
	  (<> (script)

  " Dropzone.options.ecmFileDropzone = { 
      \"maxFilesize\" : 10000 \,
       init: function() {
         $(\".attach-buttons\").hide()
          this.on(\"success\", function(file) {
            $(\".attach-buttons\").show() ;
           });
        }
}
   ;")
	  (<> (div :class "dz-message")
	    (<> "Drop files here or click to select"))
	  (<> (div :class "fallback")
	    (<> (table :align "center")
	      (<> (th) "File to Attach")
	      (<> (td)
		
		(<> (input :type "file" :name "file")))))
	  (<> (input :type "hidden" :name "gensym" :value attachment-gensym)))
	(<attachment-form> (:claim-id claim-id)
	  (<> (input :type "hidden" :name "gensym" :value attachment-gensym))
	  
	  (<> (div :class "row attach-buttons")
		     
		     (<> 'hr)
	     (<> (button :type "submit" :class "btn btn-success"
			 :style "float:left")
	       (<> "Attach"))
	     
	     (<> (a :href (cat "/ecm/claim/" claim-id)
		    :class "btn btn-danger"
		    :style "float:right")
	       (<> "Cancel")))
	   (<> (div :class "row")
	    (<> 'hr)
	    (<> (div :class "col-md-3")
	      (<> 'h3 (<> (:text "Description:"))))
	    (<> (div :class "col-md-9")
	      (<> (textarea :name "description"
			    :class "form-control"
			    :rows 3
			    :cols 80
			    :style "width:100%;")
		(when description (<> :text description)))))
	    (<> (div :class "row")
	    (<> 'hr)
	    (<> (div :class "col-md-3")
	      (<> 'h3 (<> (:text "Date:"))))
	    (<> (div :class "col-md-9")
	      (<> (input :type "text" :name "date"
				  :class "datepicker form-control"
				  :size 26
				  :value (if date
					     (ecm/ui/utility:format-timestring date)
					     (ecm/ui/utility:format-timestring
					      (ecm/local-time:format-rfc3339-timestring
					       t (ecm/local-time:universal-to-timestamp
						  (get-universal-time)))))))))
	   (<> (div :class "row")
	    
	     (<> 'hr)
	     (<> (button :type "submit" :class "btn btn-success attach-buttons"
			 :style "float:left")
	       (<> "Attach"))

	     (<> (a :href (cat "/ecm/claim/" claim-id)
		    :class "btn btn-danger"
		    :style "float:right")
	       (<> "Cancel")))
	   (<> (script)
	     "$(function() { $(\"#attachmentForm\").submit(function() {$(\"body\").addClass(\"loading\");}) }) ;")
	   (<> (style)
		"/* Start by setting display:none to make this hidden.
   Then we position it in relation to the viewport window
   with position:fixed. Width, height, top and left speak
   for themselves. Background we set to 80% white with
   our animation centered, and no-repeating */
.loading-modal {
    display:    none;
    position:   fixed;
    z-index:    1000;
    top:        0;
    left:       0;
    height:     100%;
    width:      100%;
    background: rgba( 255, 255, 255, .8 ) 
                url('http://i.stack.imgur.com/FhHRx.gif') 
                50% 50% 
                no-repeat;
}

/* When the body has the loading class, we turn
   the scrollbar off with overflow:hidden */
body.loading {
    overflow: hidden;   
}

/* Anytime the body has the loading class, our
   modal element will be visible */
body.loading .loading-modal {
    display: block;
}")
	   (<> (div :class "loading-modal") "")
	   )))))


  
  
    
	    
			    
(defun <link-to-attachment>
    (attachment
     &key (class "")
       (thunk (lambda ()
		(<> :text (getjso "description" attachment) " : ")
		(<> (small))
		(<> :text (getjso "file_name" attachment)))))
  (<> (a :href (concatenate 'string "/ecm/view?attachment="
			    (princ-to-string (getjso "_id" attachment))
			    "&go-back-to-claim="
			    (princ-to-string (getjso "claim_id" attachment)))
	 :data-ecm-link-to-attachment (princ-to-string (getjso "_id" attachment))
	 :class class)
    (funcall thunk)))
    

(defun <attachment-tr> (attachment
			 &key 
			   (claim-id (getjso "claim_id" attachment)))
  (let* ((date (getjso "date" attachment))
	 (description (getjso "description" attachment))
	 (file-name (getjso "file_name" attachment)))
    (<> (tr :style "border-bottom: none;")
      (<> (td :class "text-center")
	(<> :text date))
      (<> (td :class "text-center")
	(when (and description
		   (not (equalp "" description)))
	  (<> (span) #+(or)(blockquote :class "blockquote")
	    (<> :text
	      description))))
      (<> (td :class "text-center")
	(<link-to-attachment>
	 attachment
	 :thunk (lambda () (<> :text file-name))))
      (<> (td :class "text-center")
      	(<attachment-buttons> (getjso "_id" attachment) claim-id
			       attachment)))))

(defun <attachment-buttons> (_id claim-id attachment)
  (let ((download (format nil "/ecm/download?claim=~A&attachment=~A"
			   (getjso "claim_id" attachment)
			   (getjso "_id" attachment))))

    (<> '(html5:div :class "center-block attachment-buttons")
      (<> '(html5:div :class "btn-group text-center mx-auto")
        (<> (html5:button :type "button"
                          :class "btn btn-secondary btn-sm dropdown-toggle"
                          :data-toggle "dropdown")
          (<> (i :class "fa fa-bars") " "))

        (<> '(html5:div :class "dropdown-menu")
          (<> '(html5:div :class "text-center")
            (<link-to-attachment>
             attachment :class "btn btn-secondary btn-sm"
             :thunk (lambda() (<> "Open")))

            (<> (html5:a :class "btn btn-secondary btn-sm"
		                     :href download)
	            (<> "Download"))
            (<> `(html5:a :class "btn btn-secondary btn-sm"
		                      :href ,(concatenate
			                            'string
			                            "/ecm/view?attachment="
			                            (princ-to-string _id)
			                            "&go-back-to-claim=" (princ-to-string claim-id))
		                      :target "_blank")
	            (<> "View"))

            (when (ecm/user:user-is-administrator-p)
	            (<> `(html5:a :class "btn btn-secondary btn-sm"
		                        :href ,(concatenate
				                            'string
				                            "/ecm/edit?attachment="
				                            (princ-to-string _id)
				                            "&go-back-to-claim="
				                            (princ-to-string claim-id)
				                            "&access[type]=claim&access[id]="
				                            (princ-to-string claim-id)))
		            (<> "Edit"))
	            (<> `(html5:a :class "btn btn-secondary btn-sm"
			                      :href ,(concatenate
				                            'string
				                            "/ecm/delete?attachment="
				                            (princ-to-string _id)
				                            "&go-back-to-claim="(princ-to-string claim-id)
				                            "&access[type]=claim&access[id]="
				                            (princ-to-string claim-id)))
		            (<> "Delete")))))))))

(defun <attachment-table> (attachments
			      &rest args
			   &key
			     (claim-id)
				   (id "ecmClaimAttachmentTable"))

  (<attachment-style>)
  (<> (table :class "table table-striped table-sm persist-area"
	     :style "background-color:white"
	     :data-persistent-class "floatingHeader floatingTr"
	     :data-persistent-header ".persist-header"
	     :id id
	     :cellspacing "0"
)
      (<> (thead :class "thead-dark")
	(<> (tr :class "persist-header")
	  (<> (th :class "text-center" ) "Date")
	  (<> (th :class "text-center") "Description")
	  (<> (th :class "text-center") "File Name")
	  (<> (th :class "text-center")
	    (<> (html5:a
           :class "btn btn-light btn-outline-secondary btn-sm"
           :id "claimCreateAttachment"
			     :style "margin-left:1em;;"
			     :href (cat "/ecm/claim/" claim-id "/create-attachment"))
	      (<> "New")))))
      (<> 'tbody
	(dolist (a attachments)
	  (apply #'<attachment-tr> a args))))

  (<link-to-attachment-modal-script>)
  (<> 'html5:script
	  "$(document).ready(function() {
 $('#" id '|').DataTable({
     "paging" : false,
     "columnDefs" : [ { orderable: false, targets: [3] }],
     "autoWidth": false,
      responsive: {
          details: {
              display: $.fn.dataTable.Responsive.display.childRowImmediate,
              type: 'none',
              target: ''
          }
      }
    });

$('#ecmClaimAttachmentTable_filter').prepend($('#claimCreateAttachment').clone());
});|))


(defun <html-attachment> (attachment 
			  &key (if-file-does-not-exist :ssconvert))
  (let ((html (ecm/entity/attachment:attachment-html-archive-absolute-pathname
	       attachment))
	(path (ecm/entity/attachment:attachment-absolute-pathname
	       attachment)))
	
   ;; (break "~A ~A" html path)
    (if (cl-fad:file-exists-p html)
	(<> :unescaped (alexandria:read-file-into-string html))
	(if (eql if-file-does-not-exist :ssconvert)
	    (progn (ecm/ssconvert:ssconvert path html :export-type "Gnumeric_html:html40frag")
		   (<html-attachment> attachment :if-file-does-not-exist :error))
	    (error "cannot convert ~A"
		   (getjso "file_name" attachment))))))

(defun <msg-attachment> (attachment 
			  &key (if-file-does-not-exist :hypermail))
  (let* ((mlist (ecm/entity/attachment:attachment-mailing-list-archive-absolute-pathname
		 attachment))
#+(or)	 (path (ecm/entity/attachment:attachment-absolute-pathname
		attachment))
	 (index (merge-pathnames "index.html" mlist)))
	
    (warn "msg ~A ~A" mlist index)
    (if (cl-fad:file-exists-p index)
	(<> :unescaped
	  (alexandria:read-file-into-string
	   index))
	(if (eql if-file-does-not-exist :hypermail)
	    (progn
	      (ecm/entity/attachment:find-or-create-mailing-list-archive
	       attachment)
	      (<msg-attachment> attachment :if-file-does-not-exist :error))
	    (error "cannot make a list from ~A"
		   (getjso "file_name" attachment))))))
	

(defun <open-attachment-page> (attachment)

  (let* ((path (ecm/entity/attachment:attachment-absolute-pathname
		attachment))
	 (type (pathname-type path))
	 (download (format nil "/ecm/download?claim=~A&attachment=~A&content-disposition=inline"
			   (getjso "claim_id" attachment)
			   (getjso "_id" attachment))))
    (if (string-equal type "pdf")
	(ecm/hunchentoot:redirect 
	 (cat "/ecm/pdf/web/viewer.html?file="
	      (drakma:url-encode download :utf-8))
	 :protocol :https)
	(with-output-to-string (sexpml:*sexpml-output*)
	  (cond ((member type '("xls" "xlsx" "ods" "html" "htm")
			 :test #'string-equal)
		 (<html-attachment> attachment))
		((member type '("msg")
			 :test #'string-equal)
		 (<msg-attachment> attachment))
		(t
		 (<> :unescaped
		   (<> (ecm/ui/page:page)
		     (<> (a :href download) "Download")
		     (<> 'hr)
		     (when (member type '("jpg" "jpeg" "png" "gif")
				   :test #'string-equal)
		       (<> (img :src download)))))))))))

(defun open-attachments-page (attachments)
  "http://stackoverflow.com/questions/1964839/how-can-i-create-a-please-wait-loading-animation-using-jquery"
  (let ((length (length attachments)))
    (<> (ecm/ui/page:page :title "Attachments")
      (<attachment-navbar> (getjso "claim_id" (first attachments))
			   (getjso "_id" (first attachments)))
      (<> (div :class "row text-center" :style "font-size:150%")
	(<> :text "You have attached the following file"
	    (if (= 1 length) "" "s") ". Please look " (if (= 1 length) "it" "them")" over before ")
	(<> (a :href (cat "/ecm/claim/" (getjso "claim_id" (first attachments))))
	  (<> :text "returning to claim #" (getjso "claim_id" (first attachments)) "."))
	(<> 'hr))
      (<> (ul :class "nav nav-tabs list-inline" :role "tablist")
	(let ((num 0)
	      (active t))
	  (dolist (a attachments)
	    (let ((class (cat "nav-link"
			      (if active
				  " active"
				  ""))))
	      (setf active nil)
	      (<> (li :class "nav-item")
		(<> (a :class class
		       :data-toggle "tab"
		       :role "tab"
		       :href (cat "#" (format nil "tab~@r" (incf num))))
		
		  (<> :text (getjso "file_name" a))))))))

	(let ((num 0)
	      (active t))
	  (<> (div :class "tab-content")
	    (dolist (a attachments)
	      (let ((id (format nil "tab~@r" (incf num)))
		    (class (if active
			       " active"
			       "")))
		(setf active nil)
		(<> (div :class (cat "tab-pane" class)
			 :id id :role "tabpanel")
		  (<> 'hr)
		  (<> :text "Date:" (getjso "date" a))
		 
		  (<> :text " Description:" (getjso "description" a))
		  (<> 'hr)

		  (<> (iframe :id (cat "iframe" id)
			      :src (cat "/ecm/attachment/" (getjso "_id" a) "/")
			      :style "float:left;width:100%;height:100%;min-height:300px; border:0px"))


		  (<> (script)
		    "
	      $('#iframe" id "').load(function () {
		 $(this).height($(this).contents().find('html').height() + 30);
		 $(this).width($('#ecmBody').width());
	       });"))))))

	(<> (a :href (cat "/ecm/claim/" (getjso "claim_id" (first attachments))))
	  (<> (i :class "fa fa-arrow-left") "")
	  (<> :text " back to claim #" (getjso "claim_id" (first attachments)))))))

    

(defun <link-to-attachment-modal-script> ()
  (<> (script)
  (ps:ps
    ($ (lambda ()
	 (flet ((toggle-persistent ()
		  ($. "[data-persistent-live]" (css "z-index" "-1"))))	  
	 (|.|
	  ($"[data-ecm-link-to-attachment]")
	  (click (lambda (e)
		   (|e.preventDefault|)
		   (toggle-persistent)
		   (let* ((id (or (|.| ($ (|.| e to-element))
				       (data "ecm-link-to-attachment"))
				  (|.| ($ (|.| e current-target))
				       (data "ecm-link-to-attachment"))))
			  (iframe (+ "<iframe src=\"/ecm/attachment/"
				     id "/\" style=\"border:none;width:100%;height:100%\"></iframe>")))
		     (console.log e)
		     #+(or)($. (|.| e to-element)
			 (css ({} :padding "0px"
				  :margin "0px")))
		     (|.| ($ "#ecmModal .modal-body") (empty))
		     (|.| ($ "#ecmModal .modal-body") (append iframe))
		     

		     (|.| ($ "#ecmModal") (modal "show"))))))

	 ($. "#ecmModal"
	     (on "hidden.bs.modal"
		 (lambda () 
		   ($. window (trigger "scroll")))))))))))

		  
