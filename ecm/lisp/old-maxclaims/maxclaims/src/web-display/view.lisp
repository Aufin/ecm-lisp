(defpackage :maxclaims/web-display/view
  (:use :cl :maxclaims/yaclml/tags
	:maxclaims/web-display/display)
  (:import-from :maxclaims/web-display/view-tabs
		#:view-page-tabs)
  (:import-from :maxclaims		
		#:call-with-app-user
		#:with-ldb
		#:with-udb
		#:object-table-name
		#:object-id
		#:app-user-log)
  (:import-from :maxclaims/ecm-description
		#:ecm-attributes
		#:attribute-label
		#:attribute-value)
  (:import-from :maxclaims/web-display/navbar
		#:navbar)
  (:import-from :maxclaims/web-display/html-page
		#:with-user-html-page
		#:get-app-user)
  (:export #:view-page
	   #:view-history-page
	   #:view-page-tabs
	   #:go-back-to-claim))
(in-package :maxclaims/web-display/view)

(defmacro %view-page ((&key uri-path object title navbar) &body body)
  `(with-user-html-page (:title ,title
			 :uri-path ,uri-path) 
     (maxclaims::with-udb 
       (when ,navbar (navbar :object ,object))
       ,@body)))

(defun log-view-object (object)
  (with-ldb 
    (when (typep object 'rofl:standard-db-access-object )
      (app-user-log 
       :log-type "VIEW"
       :log-info (maxclaims::with-adb
		   (maxclaims/text-display:display object :heading))
     
       :row-type (ignore-errors (object-table-name object))
       :row-id  (if (ignore-errors 
		      (c2mop:slot-boundp-using-class 
		       (class-of object) object 
		       (rofl::class-id-slot-definition 
			(class-of object))))
		    (maxclaims::object-id object)
		    0)))))

(defun tab-id (string)
  (substitute-if-not #\- #'alpha-char-p string))

(defun go-back-to-claim (claim-number &key (text "") (tag "h1")
					(style "float:left ; 
  margin-top: 20px;
  bottom: 0;
  position: fixed;"))
  (when claim-number 
    (<:div 
     :style style
     :class "back-to-claim"   
     (<:style ".icon-x2{
    -webkit-transform:scale(2.0);
    -moz-transform:scale(2.0);
    -o-transform:scale(2.0);
}
.icon-x3{
    -webkit-transform:scale(3.0);
    -moz-transform:scale(3.0);
    -o-transform:scale(3.0);
}


      /* Sticky footer styles
      -------------------------------------------------- */

      html,
      body {
        height: 100%;
        /* The html and body elements cannot have any padding or margin. */
      }

      /* Wrapper for page content to push down footer */
      #wrap {
        min-height: 100%;
        height: auto !important;
        height: 100%;
        /* Negative indent footer by it's height */
        margin: 0 auto -60px;
      }
      
")
	(<:br)
	(<:a 
	 :href (concatenate 'string "/ecm/view?claim=" (string claim-number))
	 :rel "prev"
	 :title (concatenate 'string "< Go back to claim #" (string claim-number))

	  (<:as-is 
	   "<"tag">"
	   '#:| &nbsp; <span class="icon icon-chevron-left icon-x2" style="margin-right:10px;" aria-hidden="true">&nbsp;</span>&nbsp;| (or text "") "</"tag">")))))

(defun view-new-claim (claim-number &key (text ) (tag "h3")
					(style "float:right ; 
  margin-top: 20px;
  bottom: 0;
  position: fixed;"))
  (when claim-number 
    (<:div 
     :style style
     :class "back-to-claim"   
     (<:style ".icon-x2{
    -webkit-transform:scale(2.0);
    -moz-transform:scale(2.0);
    -o-transform:scale(2.0);
}
.icon-x3{
    -webkit-transform:scale(3.0);
    -moz-transform:scale(3.0);
    -o-transform:scale(3.0);
}


      /* Sticky footer styles
      -------------------------------------------------- */

      html,
      body {
        height: 100%;
        /* The html and body elements cannot have any padding or margin. */
      }

      /* Wrapper for page content to push down footer */
      #wrap {
        min-height: 100%;
        height: auto !important;
        height: 100%;
        /* Negative indent footer by it's height */
        margin: 0 auto -60px;
      }
      
")
	(<:br)
	(<:a
	 :id "ecmViewNewInterface"
	 :target "_blank"
	 :href (concatenate 'string "/ecm/claim/" (string claim-number))
	 :title (concatenate 'string "View #" (string claim-number) " Using New Interface. If there are any issues, or something you want added, email <a href=\"mailto:drewc.ca\">Drew Crampsie &lt;me@drewc.ca&gt;</a>. Enjoy! " )

	  (<:as-is 
	   "<"tag">"
	   '#:| &nbsp; <span class="icon  icon-eye-open icon-x2" style="margin-right:10px;" aria-hidden="true">&nbsp;</span>&nbsp;| (or text "View Using New Interface") "</"tag">"))
	(<:script (<:as-is "
$(function () {$('#ecmViewNewInterface').tooltip(
                   {'html' : true, delay: { show: 0, hide: 2000 }}
                 )
});")))))
  
(defun view-page (object 
		  &key 
		    (layers :view)
		    go-back-to-claim
		    (navbar t)			   
		    (active-tab :first)
		    (offsets nil)
		    (http-query-string)
		    (tabs-inline nil)) 
  (let ((*standard-objects-displayed* 
	 (cons object *standard-objects-displayed*)))
    (%view-page 
	(:object object
		 :title (maxclaims::with-adb 
			  (concatenate 
			   'string 
			   (maxclaims/text-display:display 
			    object :heading) 
			   " | " "ECM View"))
		 :navbar navbar
		 :uri-path http-query-string)
      (<:div 
       :id "wrap"
      (log-view-object object)
      (<:div 
       :class "container"
       (go-back-to-claim  go-back-to-claim
			 :style "float:left; top:-10px;"
			 :tag "span")
       (display object :heading)
       (<:hr)
       (<:div 
	:class "container-fluid"
	(let ((as (maxclaims/ecm-description::ecm-attributes 
		   object layers)))
	  (dolist (a as)
	    (let ((label (maxclaims/ecm-description::attribute-label a))
		  (value (maxclaims/ecm-description::attribute-value a))
		  (alayers (maxclaims/ecm-description::attribute-layers a))
		  (active (maxclaims/ecm-description::attribute-active a))
		  (prepend (getf (rest a) :prepend)))
	      (when (not (and (eq :when active)
			      (null value)))
		
		(<:div 
		 :class "row-fluid"
		 (<:div 
		  :class "span2"
		  (<:span :class "label"
			  (<:as-html label)))
		 (<:div 

		  :class "span10"
		  (<:span 
		   :class "attribute"
		   
		   ;; (break "~A ~A ~A" label (getf  (rest a) :attributes) value)
		   (flet ((doit ()
			    (if (or (eq T value)
				    (and (eq NIL value)
					 (not (getf (rest a) :as-table))))
				(<:as-html (if value "Yes" "No"))
				(apply #'display value alayers 
				       :attributes (getf  (rest a) :attributes)
				       :label nil 
				       :activate (getf  (rest a) :activate)
				       (rest a)))))
		     (if prepend
			 (<:span 
			  :class ""
			  :style "height:100%; padding:0.5em; padding-left:0px;
                                  border:1px solid #dddddd"
			  (<:span 
			   :style "padding:0.5em;background-color:#eeeeee"
			   (<:as-html prepend)) 
			  (<:span 
			   :style "padding:0.5em"
				 
				  (doit)))
			 (doit)))
		      
		     
		   (<:as-html " ")))))))))
       (if tabs-inline 
	   (view-page-tabs 
	    object 
	    :offsets offsets
	    :inline t)
	   (destructuring-bind (name . value)
	       (object-name-and-pkey-value object)
	     (<:script 
	      (<:as-is "
function setIframeHeight(iframe) {
    if (iframe) {
        var iframeWin = iframe.contentWindow || iframe.contentDocument.parentWindow;
        if (iframeWin.document.body) {
            iframe.height = iframeWin.document.documentElement.scrollHeight || iframeWin.document.body.scrollHeight;
        }
    }
};
$(window).load(function () {
    setIframeHeight(document.getElementById('view-tabs'));
});"
		       ))
	     (<:iframe 
	      :id "view-tabs"
	      :frameborder 0
	      :style "
min-height:500px;
width:100%;
overflow:hidden;"
	      :src (format nil "view-tabs?~A=~A~{&~{~A=~A~}~}"
			   name (or value -1) `(("tab[active]" 
						 ,active-tab)))
)

	 
	     (<:hr)
	     (<:iframe 
	      :style "border:none;
min-height:100px;
max-height:200px;width:100%"
	      :src (format nil "history?~A=~A~{&~{~A=~A~}~}"
			   name value (when active-tab
					`(("tab[active]" 
					   ,active-tab)))))))
       
       ))

      (when (and (typep object 'maxclaims::claim)
		 (not go-back-to-claim))
	(view-new-claim
	 (princ-to-string (maxclaims::claim.claim-id object))))
      (go-back-to-claim go-back-to-claim
			:text (concatenate 
			       'string "Back to Claim #" go-back-to-claim)
			:tag "h4")
      
)))
  
(defun view-history-page (&optional (condition NIL))
  (%view-page 
      (:navbar t
       :title "ECM: View History")
    
    (<:div 
     :class "container" 
     (when condition 
      (<:div 
       :class "text-error"
       (<:h1 (<:as-html "Error :"))
       (<:pre 
	(<:as-html (princ-to-string condition)))))
     (<:h1 (<:as-html "Recent Views"))
     (view-page-tabs (get-app-user) :view-tab :history))))
    
