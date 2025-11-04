(defpackage :maxclaims/web-display/search
  (:use :cl :maxclaims/yaclml/tags
	:maxclaims/web-display/display
	:maxclaims/web-display/navbar)  
  (:import-from :maxclaims/data-entity/app-user
		#:app-user-rolename)
  (:import-from :maxclaims
		#:with-udb
		#:search-records
		#:select-objects
		#:call-with-app-user)
  (:import-from :maxclaims/web-display/html-page
		#:with-user-html-page
		#:get-app-user)
  (:import-from :maxclaims/hunchentoot
		#:redirect)

  (:export #:search-page))

(in-package :maxclaims/web-display/search)

(defmethod activate-lambda
    ((n (eql 'maxclaims/ecm-description:search-link)))
  (lambda (log-info f)
    (declare (ignore f))
    (<:a :href (concatenate 'string "search?q="
			    (subseq 
			     log-info 12 
			     (or 
			      (position #\: log-info) 
			      (length log-info))))
	 (<:as-html (string-capitalize log-info)))))

(defun call-with-search-body (fn)
  (navbar :active "Search")
  (<:div 
   :class "container" 
   (funcall fn)))

(defun search-form ()
  
  (<:h1 "Search")
  (<:form  
   :style "display:inline!important;"
   :action "/ecm/search"		 
   (<:input 
    :class "input-large search-query"
    :name "q"
    :placeholder "Enter Search Term")
   (<:as-html " ")
   (<:button :type "submit" :class "btn-mini btn"
	     (<:as-html "Search"))))

(defun search-page (query)
  (when (string= query "")
    (setf query nil))
  (with-user-html-page (:title 
			   (apply #'concatenate 'string 
				  query `(,@(when query '(" |"))
					    "ECM search")))
       (call-with-app-user 
	(get-app-user)
	(lambda () 
	  (maxclaims::with-udb 
	    (let* ((results (when query (search-records query :exact t)))
		   (results (or results 
				(when query 
				  (search-records query :exact nil)))))
	      (when query (maxclaims::app-user-log 
			   :log-type (if results "SEARCH" "SEARCH UNSUCCESSFUL")
			   :log-info (format nil "results for ~A : (~A) " 
					     query (length results))))
	      (call-with-search-body
	       (lambda ()
		 (if query 
		     (if (= 1 (length results))
			 (let ((name.value 
				(maxclaims/web-display/display:object-name-and-pkey-value (first results))))
			   (redirect (format nil "/ecm/view?~A=~A"
					     (car name.value)
					     (cdr name.value))))
			 (progn 

			   (<:h1 (<:as-html "Results for " query " :"))
			   (<:ul 
			    :style "background-color:#EFEDEF;padding:1px;"
			    :class "unstyled"
			    (if (not results)
				(<:as-html "None")
				(dolist (r results)
				  (<:li 
				   :style "background-color:white;margin:2px; padding:2px" 
				   (<:p :class "lead"
					:style "margin-bottom:5px;"
					(<:as-html (string-capitalize (type-of r)) ": ")
					(<view-link r (display r :inline)))
				   (display r :inline-block)
				   ))))
			   (search-form)))
		     (progn
		       (search-form)
		       (let* ((user-role (app-user-rolename (get-app-user)))
			      (recent (maxclaims::with-ldb 
					(postmodern:query 
					 (format nil "
SELECT * FROM (SELECT DISTINCT ON (log_info) log_info, log_time 
               FROM user_log 
               WHERE log_type ILIKE 'search' 
               AND user_role = '~A'
               ORDER BY log_info, log_time DESC) AS log 
               ORDER BY log_time DESC LIMIT 25;"
						 user-role)
					 :rows))))
			 (when recent
			   (<:h1 (<:as-html "Recent Searches"))
			   (<:ul 
			    :style "background-color:#EFEDEF;padding:1px;"
			    :class "unstyled"
			    (dolist (r recent)
			      (<:li 
			       :style "background-color:white;margin:2px; padding:2px;" 
			       (<:a :href (concatenate 'string "/ecm/search?q="
						       (subseq 
							(first r) 12 
							(or 
							 (position #\: (first r)) 
							 (length (first r)))
							))
				    (<:as-html (string-capitalize (first r))))
			       ))))
			 ))))))))))
  )


