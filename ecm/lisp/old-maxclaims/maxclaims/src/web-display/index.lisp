(defpackage :maxclaims/web-display/index
  (:use :cl :maxclaims/yaclml/tags
	:maxclaims/web-display/display)
  (:import-from :maxclaims		
		#:call-with-app-user
		#:app-user
		#:$app-user
		#:with-ldb
		#:with-udb)
  (:import-from :maxclaims/data-entity/app-user
		#:app-user-read-only-p
		#:app-user-administrator-p)
  (:import-from :maxclaims/web-display/navbar
		#:navbar)
  (:import-from :maxclaims/web-display/html-page
		#:get-app-user
		#:with-user-html-page)
  (:import-from :maxclaims/web-display/view
		#:view-page-tabs)
  (:export #:index-page))
(in-package :maxclaims/web-display/index)

(defmacro %index-page ((&rest args) &body body)
  `(with-user-html-page (,@args)
     (maxclaims::with-udb 
       (navbar :active nil)
       ,@body)))

(defun render-create-list ()
;  (<:as-html (hunchentoot:headers-in*))
  (<:ul    
   (when nil #+(or)(app-user-administrator-p (get-app-user))
     (<:li 
      (<:a :href "create?create[type]=app-user&access[administrator]=true"
	   (<:as-html "Create new User for ECM"))))
   (<:li 
    (<:a :href "create?create[type]=person&access[read-only]=false"
	 (<:as-html "Create new Person/Company")))
   
   (<:li  
    (<:a :href "create?create[type]=risk&access[read-only]=false"
	 (<:as-html "Create new Risk")))
   (<:li 
    (<:a :href "create?create[type]=policy&access[read-only]=false"
	 (<:as-html "Create new Policy")))
   
   (<:li 
    (<:a :href "create?create[type]=contract&access[read-only]=false"
	 (<:as-html "Create new Contract")))))

(defun index-page (&key offsets
		     http-query-string)
  (%index-page 
      (:title "ECM Index"
       :uri-path http-query-string)
    #+(or)(unless (app-user-read-only-p $app-user)
      (<:script 
       :type "text/javascript"
       (<:as-html  (format nil "window.open('/ecm/diary');" ))))
    (<:div 
     :class "container"
    (<:div 
      (<:div 
       (<:h3 "Welcome to ECM ")
       #+(or)(display (get-app-user)
		            :inline ))

      (unless (app-user-read-only-p (get-app-user))
	      (render-create-list))
      (<:iframe :src "/ecm/timezone"
		            :frameborder 0
		            :height "30px"
		            :style "display:inline-block"))
      
      (destructuring-bind (name . value)
	        (object-name-and-pkey-value (get-app-user))
	      (<:iframe
	       :frameborder 0
	       :style "border:0px; min-height:600px; max-height:1024px;width:100%"
	       :src (format nil "view-tabs?~A=~A~{&~{~A=~A~}~}"
		                  name value `(
				                           ("tab[view-tab]"
				                            ,:index))))

	 
	      (<:hr)
       	)
      (<:br)
      (display (with-udb (postmodern:query (:select (:now)))) :date-time))))

