(defpackage :maxclaims/web-display/view-tabs
  (:use :cl :maxclaims/yaclml/tags
	:maxclaims/web-display/display)
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
  (:export #:view-tabs-page
	   #:view-page-tabs))

(in-package :maxclaims/web-display/view-tabs)

(defmethod maxclaims/web-display/display:display 
    :around ((object maxclaims::claim) (layer (eql :thread))
	     &key 
	       &allow-other-keys)
  (<:script 
   (<:as-is "
function _setIframeHeight(iframe) {
    if (iframe) {
        var iframeWin = iframe.contentWindow || iframe.contentDocument.parentWindow;
        if (iframeWin.document.body) {
            iframe.height = (iframeWin.document.documentElement.scrollHeight || iframeWin.document.body.scrollHeight)
        }
    }
};

var this_is_true = false;
var never_refresh = false;

function scrollClaimThread () {
   var thing = $(document).contents().find('#claimThread').contents().find('title').text() ;
   if (thing !== 'Make Thread' && thing !== '' && this_is_true && ! never_refresh) {
     parent.scroll(0,parent.document.getElementById('view-tabs').offsetParent.offsetTop);

 } else { never_refresh = true }
};
  
$(window).load(function () {
    _setIframeHeight(document.getElementById('claimThread'));
});"))

  
  (<:as-is
   (concatenate
    'string (string '|<iframe id="claimThread" name="claimThread"frameborder="0" width="100%" style="min-height:500px; width:100%; overflow:hidden;" onload="scrollClaimThread(); this_is_true = true;" src="|)
    
    (format nil "/ecm/claim/~A/thread/"
	    (maxclaims::claim.claim-id object))
    (string '|"></iframe>|)))

  )

(defmethod maxclaims/web-display/display:display 
    :around ((object maxclaims::claim) (layer (eql :diary))
	     &key 
	       &allow-other-keys)
  (<:script 
   (<:as-is "
function _setIframeHeight(iframe) {
    if (iframe) {
        var iframeWin = iframe.contentWindow || iframe.contentDocument.parentWindow;
        if (iframeWin.document.body) {
            iframe.height = (iframeWin.document.documentElement.scrollHeight || iframeWin.document.body.scrollHeight)
        }
    }
};

var this_is_true = false;
var never_refresh = false;

function scrollClaimDiary () {
   var thing = $(document).contents().find('#claimDiary').contents().find('title').text() ;
   if (thing !== 'Make Diary' && thing !== '' && this_is_true && ! never_refresh) {
     parent.scroll(0,parent.document.getElementById('view-tabs').offsetParent.offsetTop);

 } else { never_refresh = true }
};
  
$(window).load(function () {
    _setIframeHeight(document.getElementById('claimDiary'));
});"))

  
  (<:as-is
   (concatenate
    'string (string '|<iframe id="claimDiary" name="claimDiary"frameborder="0" width="100%" style="min-height:500px; width:100%; overflow:hidden;" onload="scrollClaimDiary(); this_is_true = true;" src="|)
    
    (format nil "/ecm/claim/~A/diary/"
	    (maxclaims::claim.claim-id object))
    (string '|"></iframe>|)))

  )
(defun tab-id (string)
  (substitute-if-not #\- #'alpha-char-p string))

(defun view-tabs-page (object 
			    &rest args)
    (let ((*standard-objects-displayed* 
	 (cons object *standard-objects-displayed*)))
      (with-user-html-page (:title "View Tabs")
	(<:style
	 (<:as-is
	 "body {
    font-family: -apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,\"Helvetica Neue\",Arial,sans-serif;
    font-size: 1rem;
    line-height: 1.5;
    color: #373a3c;
    background-color: #fff;
}"))
      (maxclaims::with-udb        
	(apply #'view-page-tabs object args)))))

(defun view-page-tabs (object 
		       &key 
			 (inline nil)
			 (offsets nil)
			 (active-tab :first)
			 (view-tab :view-tab))
  (<:div 
   :class "container-fluid"
   (<:script 
    (<:as-is "$(window).load(function () {
       $('a[data-toggle=\"tab\"]').on('shown', function (e) { 
           parent.setIframeHeight($('#view-tabs', window.parent.document)[0])
       });
})"))
   (let ((as (loop :for a :in (maxclaims/ecm-description::ecm-attributes 
			       object view-tab) 
		:nconc
		(let* ((tab-value-layer (maxclaims/ecm-description::attribute-value a))
		       (active (maxclaims/ecm-description::attribute-active a))
		       (tab-attributes 
			(maxclaims/ecm-description::ecm-attributes 
			 object tab-value-layer)))
		  (when active
		    (if (eq :when active)
			(when (loop 
				 :for ta :in tab-attributes 
				 :when (maxclaims/ecm-description::attribute-value ta)
				 :collect 1)
			  (list a))
			(list a)))))))
     (let ((class "active"))
       ;; * We duplicate the header so the fixed works yet the content is in the right place
       (flet ((header (&optional (css ""))
		(<:ul 
		 :class "nav nav-tabs"
		 :style (concatenate 'string css 
				     "
opacity:0.9;
background-color:white;
")
		 :id "myTab"
		 (when (or  (string-equal 
			     active-tab "first")
			    (not active-tab))
		   (setf active-tab :first))
		 (let ((active-tab active-tab))
		   (dolist (a as)
		     (let ((*tab-id* (tab-id (maxclaims/ecm-description::attribute-label a))))
		   
		  
		       (<:li 
			:class (if (string-equal active-tab *tab-id*)
				   "active" 
				   (if (or (string-equal 
					    active-tab "first")
					   (eq active-tab :first))
				       (prog1 "active"
					 (setf active-tab nil))
				       ""))
			(<:a 
			 :href (format nil "#~A" *tab-id*)
			 :data-toggle "tab" 
			 (<:as-html (maxclaims/ecm-description::attribute-label a))))
		       (setf class "")))))))
	 (when (not inline)
	   (header "position:fixed;"))
	 (header))
       
	    (setf class "active in")
	    (<:div 
	     :id "myTabContent"
	     :class "tab-content"
	     (let ((active-tab active-tab))
	       
	       (dolist (a as)
		 (let* ((value (maxclaims/ecm-description::attribute-value a))
			(tab-id (tab-id (maxclaims/ecm-description::attribute-label a)))
			(maxclaims/web-display/display::*current-tab* tab-id)
			(maxclaims/ecm-description::*query-offset*
			 (or (ignore-errors 
			       (parse-integer (cadr (assoc tab-id offsets :test #'string-equal))))
			       
			     0)))
		 
		   (<:div 
		    :class (concatenate 'string "tab-pane fade " 
					(if (string-equal active-tab tab-id)
					    "active in" 
					    (if (eq active-tab :first)
						(prog1 "active in"
						  (setf active-tab nil)) 
					"")))
		    :id tab-id		   
		    (let ((tab-attributes 
			   (maxclaims/ecm-description::ecm-attributes 
			    object value)))	      		     
		      (dolist (ta tab-attributes)
			(let ((label (maxclaims/ecm-description::attribute-label ta))
			      (value (maxclaims/ecm-description::attribute-value ta))
			      (alayers (maxclaims/ecm-description::attribute-layers ta)))
			  (<:div 
			   :class "row-fluid"
			   (when label 
			     (<:div 
			      :class "span2"
			      (<:span :class "label"
				      (<:as-html label))))
			   (<:div 
			    :class (if label "span10" "span12")
			    (<:span 
			     :class "attribute" 
			     (apply #'display value alayers 
				    :attributes (getf  (rest ta) :attributes)
				    :label nil 
				    :activate (getf  (rest ta) :activate)
				    (rest ta)))
			    (<:as-html " "))))))))
		 (setf class ""))))

	    (<:hr)))))
