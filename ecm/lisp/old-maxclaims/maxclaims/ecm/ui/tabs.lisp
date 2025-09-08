(defpackage :ecm/ui/tabs
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.| #:$.)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/ui/diary)
  (:import-from :ecm/ui/thread)
  (:import-from :ecm/ui/transaction)
  (:import-from :ecm/ui/timecard)
  (:import-from :ecm/ui/attachment)
  (:import-from :ecm/ui/iframe)
  (:import-from :ecm/ui/utility #:cat)
  (:export #:<claim-tabs>))
(in-package :ecm/ui/tabs)


(defun <claim-tabs> (claim-crux &key (active "Transactions")
				  (ui-tabs t))
  (<> (div :class "card-header clearfix persist-header"
	   :role "tab" :id "ClaimTabsHeading"
	   :style "margin-bottom:0px;padding-bottom:0px;border-bottom:0px;white-space:nowrap;")
    (<> (div :class "hover-btn")
      (<> (a :class "close ecm-window-minimize"
	     :data-toggle "collapse"
	     :href "#claimTabs"
	     :data-claim-ui "tabs"
	     :aria-expanded "true"
	     :aria-controls "claimTabs")
	(<> (html5:i :class (cat "fa "
				 (if ui-tabs
				     "fa-caret-square-o-up"
				     "fa-caret-square-o-down"))
		     :aria-hidden "true")
	  " ")))
					;   ; (break "~A" active)
    (<> (ul :class "nav nav-tabs list-inline" :role "tablist")
      (<> (li :class "nav-item")
	      (<> (a :class (cat "nav-link"
			                     (when (string-equal active "Transactions")
			                       " active"))
	             :data-ecm-tab "Transactions"
	             :data-toggle "tab"
	             :href "#ecmClaimTransactionTab"
	             :role "tab")
	        "Transactions"))
      (<> (li :class "nav-item")
	      (<> (a :class (cat "nav-link"
			                     (when (string-equal active "Timecards")
			                       " active"))
	             :data-ecm-tab "Timecards"
	             :data-toggle "tab"
	             :href "#ecmClaimTimecardTab"
	             :role "tab")
	        "Timecards"))
      (<> (li :class "nav-item")
	      (<> (a :class (cat "nav-link"
			                     (when (string-equal active "Diary")
			                       " active"))
	             :data-ecm-tab "Diary"
	             :data-toggle "tab"
	             :href "#ecmClaimDiaryTab"
	             :role "tab")
	        "Diary"))
      (<> (li :class "nav-item")
	      (<> (a :class (cat "nav-link"
			                     (when (string-equal active "Attachments")
			                       " active"))
	             :data-ecm-tab "Attachments"
	             :data-toggle "tab"
	             :href "#ecmClaimAttachmentsTab"
	             :role "tab")
	        "Attachments"))
      #+(or)(<> (li :class "nav-item")
	            (<> (a :class (cat "nav-link"
			                           (when (string-equal active "Thread")
			                             " active"))
	                   :data-toggle "tab"
	                   :href "#ecmClaimThreadTab"
	                   :role "tab")
	              "Thread"))
      (<> (li :class "nav-item")
	      (<> (a :class "nav-link"
	             :data-toggle "tab"
	             :href "#ecmOldTabs"
	             :role "tab")
	        "Old Tabs"))))
  (<> (div :id "claimTabs" :class (cat "collapse"
				       (when ui-tabs " show"))
	   :role "tabpanel"
	   :data-claim-ui "tabs"
	   :aria-labelledby="claimTabsHeading")
    (<> (div :class "card-block")	    
      (<> (div :class "tab-content")
	
	;; ** Transaction
	(<> (div :class (cat "tab-pane"
			     (when (string-equal active "Transactions")
			       " active"))
		 :id "ecmClaimTransactionTab"
		       :role "tabpanel")
    (<> (html5:div :class "mx-auto")
	    (ecm/ui/transaction::<claim-transaction-table>
	     (ecm/json:getjso "transactions" claim-crux)
	     :claim-id (ecm/json:getjso "_id" claim-crux))))

	;; ** Timecards
	(<> (div :class (cat "tab-pane"
			     (when (string-equal active "Timecards")
			       " active"))
		 :id "ecmClaimTimecardTab"
		 :role "tabpanel")
	  (ecm/ui/timecard::<claim-timecard-table>
	   (ecm/json:getjso "timelog" claim-crux)
	   :claim-id (ecm/json:getjso "_id" claim-crux)))

	;; ** Attachments
	(<> (div :class "tab-pane" :id "ecmClaimAttachmentsTab"
		 :role "tabpanel")
	  (ecm/ui/attachment:<attachment-table>
	   (ecm/json:getjso "attachments" claim-crux)
	   :claim-id (ecm/json:getjso "_id" claim-crux)))
	;; ** Thread
	#+(or)(<> (div :class "tab-pane" :id "ecmClaimThreadTab"
		 :role "tabpanel")
	  (<claim-tab-thread> (ecm/json:getjso "_id" claim-crux)))

	;; ** Diary
	(<> (div :class "tab-pane" :id "ecmClaimDiaryTab"
		 :role "tabpanel")
	  (<claim-tab-diary>
	   (ecm/json:getjso "_id" claim-crux)
	   (ecm/json:getjso "diary" claim-crux)))
	;; ** Old Tabs
	(<> (div :class "tab-pane" :id "ecmOldTabs"
		 :role "tabpanel")
	  (<claim-tab-iframe> (ecm/json:getjso "_id" claim-crux))
	  (<claim-tab-script> (ecm/json:getjso "_id" claim-crux)))))))

(defun <claim-tab-script> (claim-id)
  (<> (script)
    (ps:ps*
     `(defun set-ecm-active-tab (active-tab)
	      (let ((ajax ({} :type "POST" 
	                      :url (+ "/ecm/ui/claim/" ,claim-id)
	                      :content-type "application/json; charset=utf-8"
	                      :data-type "json"
	                      :data (|JSON.stringify| ({} "active_tab" active-tab)))))
                                        ;	  (console.log ajax)
	        ($.ajax ajax)))
     `($ (lambda ()
	         (|.| ($ "a[data-toggle=\"tab\"]")
	              (on "shown.bs.tab"
		                (lambda (e)
		                  (set-ecm-active-tab (|.| ($ e.target) (data "ecm-tab")))
                      (let ((tabytab ($. e.target (attr "href"))))
                        ($. tabytab
                            (find ".dataTable")
                            (each (lambda (i v)
                                    ($. v (|dataTable|) (api) columns (adjust)(draw))))))))))))))
  

(defun <claim-tab-iframe> (claim-number &key active-tab)
  (<> '(html5:div :class "row")
    (<> '(html5:div :class "col-sm-12")
      (<> '(html5:div :id "ecmTabLoadingSpinner"
	    :style "text-align:center; width:100%; margin:auto" )
	(<> :unescaped '#:|<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
<span class="sr-only">Loading...</span>|))
      (<> `(html5:iframe
	    :id "viewTabs"
	    :frameborder 0
	    :onload "document.getElementById('ecmTabLoadingSpinner').style.display='none';"
	    :style "min-height:500px; width:100%; overflow:hidden;"
	    :src ,(format nil "/ecm/view-tabs?tab[view-tab]=CLAIM-VIEW-TAB&claim=~A~{&~{~A=~A~}~}"
			  (or claim-number -1)
			  (when active-tab `(("tab[active]" 
					      ,active-tab))))))))
  (ecm/ui/iframe:<iframe-load-resize> "#viewTabs"))

(defun <claim-tab-thread> (claim-number)

  (<> :unescaped 
      (string '|<iframe id="claimThreadIframe" name="claimThreadIframe" frameborder="0" width="100%" style="min-height:500px; width:100%; overflow:hidden;" src="|)
    (format nil "/ecm/claim/~A/thread/"
	    claim-number)
    (string '|"></iframe>|))
  (ecm/ui/iframe:<iframe-load-resize> "#claimThreadIframe"))

(defun <claim-tab-diary> (claim-id entries)
  (ecm/ui/diary:<diary-entries-table>
   entries
   :claim-id claim-id
   :claim-number nil
   :schedule nil
   :deferred nil))

(defun <claim-tabs-page> (claim-crux &rest args )
  (<> '(ecm/ui/page:page :title "Tabs")
    (apply #'<claim-tabs> claim-crux args)))
  


