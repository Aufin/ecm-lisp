(defpackage :maxclaims/web-display/navbar
  (:use :cl :maxclaims/yaclml/tags
	:maxclaims/web-display/display)
  (:import-from :maxclaims #:user-can-edit-p
		#:user-read-only-p)
  (:export #:navbar))
(in-package :maxclaims/web-display/navbar)

(defun navbar (&key (active "View")
		             (object NIL)
		             (class "navbar-static-top"))
  (let ((nav `(("Search" "/ecm/search")
	             ("View" "/ecm/view")
	             ("Reports" "/ecm/report"))))
    
    (<:div 
     :class (concatenate 'string "navbar " class)
     (<:div 
      :class "navbar-inner"
      (<:div 
       :class "container"    
       (<:a :class "brand" :href "/ecm/index" "ECM"
            )


       (<:ul 
	      :class "nav"
	      (dolist (n nav)
          (when (or (not (equalp "View" (first n)))
                    object)
	          (<:li :class (if (equalp active (first n))
			                       "active"
			                       "")
		              (if (and object (equalp "View" (first n))
			                     (cdr
			                      (ignore-errors
			                       (object-name-and-pkey-value object))))
		                  (<view-link object "View")
		                  (<:a :href (second n) (<:as-html (first n)))))))

	      (when (and object (user-can-edit-p object))
	        (<:li
	         :class (if (equalp active "Edit")
		                  "active"
		                  "")
	         (if (cdr (ignore-errors  (object-name-and-pkey-value object)))
	             (<edit-link object (<:as-html "Edit"))
	             (<:a :href "#" (<:as-html "Edit")))))
	      (unless (user-read-only-p (get-app-user))
	        (<:li
	         :class (if (equalp active "Diary")
		                  "active"
		                  "")
	         (<:a :href "/ecm/diary" (<:as-html "Diary"))))
	   
	
	      #+(or)(<:li
	       (<:a :href
	            (format nil
		                  "message?create[type]=app-user-message&attribute[from]=~A" (maxclaims::app-user.app-user-id (get-app-user)))
	            (<:as-html "Message")))

	      (when (maxclaims::app-user.admin (get-app-user))
	        (<:li 	   :class (if (equalp active "Manage")
				                        "active"
				                        "")
			               (<:a
			                :href "manage"
			                (<:as-html "Manage")))
	        #+(or)(<:li
	         :class (if (equalp active "Todo")
		                  "active"
		                  "")
	         (<:a
	          :href "todo"
	          (<:as-html "Todo"))))
	      (<:li
	       (<:a
	        :href "logout"
	        (<:as-html "Logout"))))
       (<:form  
	      :class "navbar-form pull-right"
	      :style "display:inline!important;"
	      :action "search"
	      (<:input
	       :class "input-large search-query"
	       :name "q"
	       :placeholder "Enter Search Term")
	      (<:as-html " ")
	      (<:button :type "submit"
		              :class "btn-mini btn"
		              :style "display:inline-block"
		              (<:as-html "Search"))))))

    (<:as-is '#:|

<div id="myModal" class="modal hide fade" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
  <div class="modal-header">
    <button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button>
    <h3 id="myModalLabel">Messagey</h3>
  </div>
  <div class="modal-body">
  </div>
  <div class="modal-footer">
    <button class="btn" data-dismiss="modal" aria-hidden="true">Close</button>
  </div>
</div>|)))


