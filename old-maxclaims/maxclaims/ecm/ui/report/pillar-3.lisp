(defpackage :ecm/ui/report/pillar-3
  (:use :cl)
  (:import-from :ecm/user #:with-user)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.|)
  (:import-from :ecm/ui/spreadsheet
		#:<spreadsheet-type-select>
		#:<download-spreadsheet>)
  #+(or)  (:import-from :ecm/report/mi #:syndicate-mi-report
		      #:syndicate-mi-report-spreadsheet)

  (:export #:pillar-3-page))
(in-package :ecm/ui/report/pillar-3)

(defun pillar-3-page (&key syndicate-id error)
  (<> '(ecm/ui/page:page :title "Pillar 3 Report : ECM")
    (when error
      (<> (code)
	(<> (blockquote) (<> :text error))))
	       
	(<> '(div :class "row row-centered report")
	  (<> 'html5:h1 "Pillar 3 Report")
	  (<> 'br))
	(<pillar-3-report-form> syndicate-id)

	(<> 'style
          (<> "iframe {
    border: none;
    resize: both;
  overflow: auto;
  width: 100%;
}"))
	
        (<> '(iframe :name "report"  
              :id "report"
              :onload "this.width=$('body').width();
                       var h = $(window).height() - $('#report').offset().top + 50;
                         $('#report').height(h);"
              :style " overflow: scroll; width:100%")
          " ")
	
	(<> (script)
	  '|$("#report").load(function() {
    $(this).height( $(this).contents().find("body").height() + 50 );
    $(this).width( $(this).contents().find("body").width() );
});|)))
  
#+(or) (hunchentoot:define-easy-handler (mi-report-handler
                                  :uri "/ecm/report/mi")
    (syndicate-id
     start-date
     end-date
     spreadsheet-type)
  (if (string-equal "POST" (hunchentoot:request-method*))
      (with-user ()
	(<download-spreadsheet>
	 (syndicate-mi-report-spreadsheet
	  syndicate-id
	  start-date
	  end-date)
	 :type spreadsheet-type))
			      
       
      ))

(defun <pillar-3-report-form> (syndicate-id)
  (<> 'style 
    (<> "/* centered columns styles */
.row-centered {
    text-align:center;
}
.col-centered {
    display:inline-block;
    float:none;
    /* reset the text-align */
    text-align:left;
    /* inline-block space fix */
    margin-right:-4px;
}"))
  (<> '(form :method "POST" 
	:action "/ecm/report/pillar-3"
	:target "report")
    (<> '(div :class "row row-centered")
      (<> '(div :class "col-md-8 col-centered")
        (<> '(div :class "row-centered")

	  ;; Syndicate
          (<> 'h3 (<> "Syndicate"))
	  (ecm/user:with-user ()
	    (<> '(select :name "syndicate-id")
	      (dolist (s (list-syndicates))
		(<> `(option :value ,(second s)
			     ,@(when (equalp (princ-to-string (second s))
					     syndicate-id)
				 (list :selected "selected")))
		  (<> :text (first s)))))))
	
	(<> 'br)))
    
    (<> '(div :class "row row-centered")
      (<> 'html5:script
        (ps:ps
          ($ (lambda ()
               ;;(|.| ($ ".report") (hide))
               (ps:chain ($ ".datepicker")
                         (datepicker))
               (ps:chain ($ ".datepicker")
                         (datepicker
                          "option" "dateFormat" "yy-mm-dd")
			 (datepicker
                          "option" "changeYear" t)
			 (datepicker
                          "option" "changeMonth" t))
               (let* ((date (ps:new (|Date|)))
                      (month (ps:chain date (get-month)))
		      (year (ps:chain date (get-full-year)))
		      (end-date (ps:new (|Date|))))
		 (|.| date (set-year (ps:decf year)))
		 (|.| date (set-date 1))
		 (|.| end-date (set-date 1))
                 (when (= "" (|.| ($ "#start-date") (val)))
		   (ps:chain ($ "#start-date")
			     (datepicker "setDate" date)))
		 (when (= "" (|.| ($ "#end-date") (val)))
		   (ps:chain ($ "#end-date")
			     (datepicker "setDate" end-date))))))))
      (<> '(div :class "col-md-4 col-centered")
        (<> '(div :class "row-centered")
          (<> 'h3 (<> "Start Date (including)"))
          (<> '(input :name "start-date"
                :class "datepicker"
                :id "start-date"))))
      (<> '(div :class "col-md-4 col-centered")
        (<> '(div :class "row-centered")
          (<> 'h3 (<> "End Date (including)"))
          (<> '(input :name "end-date"
                :class "datepicker"
                :id "end-date")))))
    
    (<> '(div :class "row row-centered report")
      (<> 'br)
      (<> '(div :class "col-xs-6 col-centered")
        (<> '(span :id "ss-type-span"
              :class "row-centered"))
	(<> '(button :type "submit" :class "btn btn-success")
	  (<> "Run Report"))
	(<> " as ")
	(<spreadsheet-type-select> 
	 :name "spreadsheet-type"
	 :class "spreadsheet-type-select")))
      
    (<> '(input :name "present[SPREADSHEET-TYPE]"
	  :value "Gnumeric_Excel:xlsx2"
	  :type "hidden"))))

(defun list-syndicates ()
  (postmodern:query "
SELECT * 
 FROM (SELECT person_name(person) AS name, person_id 
       FROM person
       WHERE person_id IN (SELECT DISTINCT syndicate_id FROM contract)) 
 AS c
 ORDER BY name"))



