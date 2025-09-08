(uiop:define-package :ecm/ui/navbar
    (:use :cl :sexpml)
  (:import-from :ecm/ui/bootstrap)
  (:import-from :ecm/ui/jquery)
  (:import-from :ecm/user #:with-user
		#:user
		#:user-can-edit-p)
  (:import-from :ecm/ml #:attribute-value)
 ; (:import-from :ecm/downtime)
  (:import-from :ecm/entity/diary #:diary-schedule-numbers)
  (:export #:navbar))
(in-package :ecm/ui/navbar)

(defun <navbar-style> ()
   (<> 'style (<> :text "
body {
  /* Show it's not fixed to the top */
  min-height: 100vh;
  padding-top: 4rem;
}

.ecm-nav {
  width:100%;
  max-width: 100vw;
  max-height: 4rem;
  font-size: calc(1.5rem + (26 - 14) * ((100vw - 300px) / (1600 - 300)));
  line-height: 1.5rem;
}

@media screen and (max-width: 1000px) {
.ecm-nav {
  font-size: calc(1.2rem + (26 - 14) * ((100vw - 300px) / (1600 - 300)));

  line-height: calc(1.1rem + (1.5 - 1.2) * ((100vw - 300px)/(1600 - 300)));
 }
}
.ecm-nav:hover .navbar-toggler {
  border: 1px solid white;

}

.ecm-nav-contents {
  width:100%;
  text-align:center;
}

.ecm-nav-buttons {
  white-space: nowrap;
  width:2rem;
  position:relative;
  right: 1em;
}

.ecm-nav .brand {
  z-index: 150;
  width: 2rem;
}






")))

(defun search-modal ()
  (<> :unescaped '|
<div class="modal fade" id="searchModal" tabindex="-1" role="dialog" aria-labelledby="searchModalLabel" aria-hidden="true">
  <div class="modal-dialog" role="document">
    <div class="modal-content">
      <div class="modal-header">
        <h4 class="modal-title" id="searchModalLabel">ECM Search</h4>
        <button type="button" class="close" data-dismiss="modal" aria-label="Close">
          <span aria-hidden="true">&times;</span>
        </button>
      </div>
 <form class="form-inline" action="/ecm/search">
      <div class="modal-body">

    <input name="q" class="form-control" type="text" placeholder="Enter Search Term" autofocus id="searchModalInput">


      </div>

      <div class="modal-footer">
<button class="btn btn-primary" type="submit">Search</button>
        <button type="button" class="btn btn-secondary" data-dismiss="modal">Close</button>
      </div>
  </form>
    </div>
  </div>
</div>
|)

  (<> (html5:script)
    "$('.modal').on('shown.bs.modal', function() {
  $(this).find('[autofocus]').focus();
});"))

(defun downtime-modal ()
  (<> :unescaped '|
<div class="modal fade" id="downtimeModal" tabindex="-1" role="dialog" aria-labelledby="searchModalLabel" aria-hidden="true">
  <div class="modal-dialog" role="document">
    <div class="modal-content">
      <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-label="Close">
          <span aria-hidden="true">&times;</span>
        </button>
        <h4 class="modal-title" id="searchModalLabel">ECM Search</h4>
      </div>
 <form class="form-inline" action="/ecm/search">
      <div class="modal-body">

        This system will be going down on 
| '|


      </div>

      <div class="modal-footer">
<button class="btn btn-secondary-outline" type="submit">Search</button>
        <button type="button" class="btn btn-secondary" data-dismiss="modal">Close</button>
           
      </div>
  </form>
    </div>
  </div>
</div>
|)

  (<> (html5:script)
    "$('.modal').on('shown.bs.modal', function() {
  $(this).find('[autofocus]').focus();
});"))



;; (sexpml:define-tag (navbar &attributes
;; 			   (type nil)
;; 			   (id nil)
;; 			   (class "navbar navbar-fixed-top navbar-dark bg-inverse")
;; 			   &body contents)

(defun diary-let*-bindings ()
  '((schedule (ecm/entity/diary:diary-schedule-numbers))
	  (overdue (ecm/json:getjso "overdue" schedule))
	  (today (ecm/json:getjso "today" schedule))
	  (tomorrow (ecm/json:getjso "tomorrow" schedule))
	  (one-week (ecm/json:getjso "one_week" schedule))
	  (future (ecm/json:getjso "future" schedule))
	  (total (ecm/json:getjso "total" schedule))
	    
	  (diary
	   (unless (every #'zerop (list overdue today tomorrow one-week future total))
	     (apply #'concatenate
		          'string "

Click Button to View
<hr>"
		          `(
		            ,@(unless (zerop overdue)
			              (list 
			               "Outstanding: "
			               (princ-to-string overdue)))
			          ,@(unless (zerop today)
			              (list
			               "Today: "
			               (princ-to-string today)
			               ))
			          ,@(unless (zerop tomorrow)
			              (list 
			               "Tomorrow: "
			               (princ-to-string tomorrow)
			              ))
			          ,@(unless (zerop one-week)
			              (list 
			               "Within One Week (from tomorrow): "
			               (princ-to-string one-week)
			              ))
			          ,@(unless (zerop future)
			              (list 
			               "After One Week (from tomorrow): "
			               (princ-to-string future)
			              ))
			
			         ))))))

(defmacro <ecm-dropdown> (type id edit?)
  `(<> (html5:div :class "dropdown-menu")
     (when ,edit?
       (<> (html5:a :href (concatenate
			                     'string "/ecm/edit?"
			                     ,type "=" (princ-to-string ,id))
		                :class "dropdown-item")
	       (<> "Edit"))
       (<> (div :class "dropdown-divider") ""))
     (<> (html5:a :href "/ecm/index"		     
		              :class "dropdown-item")
	     (<> :text "Home"))
     (when (and ,type ,id)
       (<> (html5:a :href (concatenate
			                     'string "/ecm/view?" ,type "="
			                     (princ-to-string ,id) "&old-view=t")		     
		                :class "dropdown-item"
		                :target "_blank")
	       (<> :text "Old View")))
     (<> (div :class "dropdown-divider") "")
     (<> (a :href "/ecm/report#"
		               :class "dropdown-item")
       (<> :text "Reports"))
     (when (ecm/user:user-is-administrator-p)
       (<> `(html5:a :href "/ecm/manage"
		                 :class "dropdown-item")
	       (<> "Administer ECM")))

     (<> (span :class "dropdown-item")
		   (<> '(button
			       :type "button"
			       :class "nav-item"
			       :title "Click to Search"
			       :data-toggle "modal"
			       :data-target "#searchModal")
		     (<> :unescaped "Search"'|<i class="fa fa-search"></i>|)))

     ))
     

(defmethod sexpml:sexpml-form ((name (eql 'navbar))
                               &key attributes contents
				 )
  (sexpml:sexpml-attributes-bind (; (active nil)
				  (type nil)
				  (id nil)
				  (class "navbar fixed-top navbar-dark navbar-expand bg-dark "))
      attributes
    (let ((typen (gensym))
	        (idn (gensym))
	        (edit? (gensym))
	        (classn (gensym)))
      `(let* ((,typen (or (attribute-value :type)
			                    ,type))
	            (,idn (or (attribute-value :id)
			                  ,id))
	            (,edit? (and ,typen ,idn (user-can-edit-p
				                                :type ,typen
				                                :id ,idn)))
	            (,classn (or (attribute-value :class)
		                       ,class))
	            ,@(diary-let*-bindings)
	            )
	       
         (<navbar-style>)
         (<> (nav :class (concatenate 'string "ecm-nav clearfix " ,classn)
		              :style "color:white;")
           (<> (span :class "brand")
	           (<> (ul :class "nav navbar-nav")
	             (<> (li "nav-item dropdown")
	               (<> (a :class "btn btn-outline-secondary btn-sm nav-item dropdown-toggle"
		                    :style "color:white"
		                    :role "button"
		                    :href "#"
		                    :data-toggle "dropdown"
		                    :aria-haspopup "true"
		                    :aria-expanded "false")
		               (<> "ECM"))
	               (<ecm-dropdown> ,typen ,idn ,edit?))
	             ))
           (<> (div :class "ecm-nav-contents")
	           ,@contents)
	         (<> (div :class "ecm-nav-buttons d-none d-md-inline-block"
		                )
             (<> '(button
			             :type "button"
			             :class "btn btn-outline-secondary btn-sm nav-item"
			             :title "Click to Search"
			             :data-toggle "modal"
			             :data-target "#searchModal"
			             :style "color:white")
		           (<> :unescaped '|<i class="fa fa-search"></i>|))

             (<> `(html5:span 
		               :data-toggle "popover" :data-trigger "hover"
		               :data-placement "left"
		               :data-offset "-190px 0px"
		               :data-original-title ,(concatenate
					                                'string "Agenda for "
					                                (princ-to-string total) " Diary Entries")
		               :data-content ,diary
		               :data-html "true"
		               :class "navbar-bg-inverse")
		           (<> `(html5:a :href "/ecm/diary"
				                     :id "ecmDiaryPopoverTrigger"
				                     :style "color:white;" :target "_blank"
				                     :class "btn btn-outline-secondary btn-sm nav-item dropdown"
				                     )

		             (<> :unescaped '|<i class="fa fa-book"></i>|)))))



	       (<> 'html5:script 
	         '|$(function () {
  $('[data-toggle="popover"]').popover()
});|) 
         (<> (div)
           (search-modal)
           (downtime-modal))))))


;; ;; * Resize body to fit navbar
;; (<> 'html5:script
;; 	  "
;; $(function() {

;; /* First we resise the window to be lower than the navbar */

;;   $('body').css('padding-top', ($('.navbar').height() + 1) + 'vh'); 

;; /* Then, when it is resized, so is the window */

;; $( window ).resize(function() { 
;;    $('body').css('padding-top', ($('.navbar').height() + 1) + 'vh'); }) ;


;;                            });

;;                            ")
       

;; ;;  


       
;; ;; (<> '(html5:div :class "collapse" :id "exCollapsingNavbar")
;; ;; 	   (<> '(html5:a :class "navbar-brand" :href "/ecm/index")
;; ;; 	   (<> "ECM"))
;; ;; 	 (<> '(html5:ul :class "nav navbar-nav")
	   
;; 	   ;; (when edi
;; 	   ;;   (<> '(html5:li :class "nav-item")
;; 	   ;;     (<> `(html5:a :href ,(concatenate
;; 	   ;; 			     'string "/ecm/edit?"
;; 	   ;; 			     type "=" (princ-to-string id))
;; 	   ;; 		     :class "nav-link")
;; 	   ;; 	 (<> "Edit"))))



;; ;; 	   (when (and type id)
;; ;; 	     (<> '(html5:li :class "nav-item")
;; ;; 	       (<> `(html5:a :href ,(concatenate
;; ;; 				     'string "/ecm/view?" type "="
;; ;; 				     (princ-to-string id) "&old-view=t")
				
;; ;; 			     :class "nav-link"
;; ;; 			     :target "_blank")
;; ;; 		 (<> :text "Old View"))))

	   ;; (when (and type id)
	   ;;  )
	   
;; ;;  (<> :unescaped '|
;; ;;   <form class="form-inline pull-xs-right" action="/ecm/search">
;; ;;     <input name="q" class="form-control" type="text" placeholder="Search">
;; ;;     <button class="btn btn-secondary-outline" type="submit">Search</button>
;; ;;   </form>


;; ;; |)))

