(defpackage :ecm/ui/report/agency-bordereau
  (:use :cl)
  (:import-from :ecm/user #:with-user)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.|)
  (:import-from :ecm/ui/spreadsheet
		#:<spreadsheet-type-select>
		#:<download-spreadsheet>)
  (:import-from :ecm/report/agency-bordereau))
(in-package :ecm/ui/report/agency-bordereau)

(defvar *agency-bordereaux-hash* (make-hash-table :test #'equalp))

(defstruct agency-bordereau
  universal-time
  (status nil)
  agency-id
  start-date
  end-date
  thread
  pathname)

(defun make-agency-bordereau-thread (ab)
  (bt:make-thread
   (lambda (&aux (bc ab))
     (setf (agency-bordereau-status bc)
	   :started)
     (setf (agency-bordereau-pathname bc)
	   (maxclaims::with-adb
	     (ecm/report/agency-bordereau:agency-bordereau-csv-file
	      (agency-bordereau-agency-id bc)
	      (agency-bordereau-start-date bc)
	      (agency-bordereau-end-date bc)
	      :universal-time (agency-bordereau-universal-time bc))))
     (setf (agency-bordereau-status bc)
	   :done))
   :name "Agency Bordereau"))

(defun new-agency-bordereau (agency-id
			     start-date
			     end-date
			     universal-time)
  (let ((bordereau (make-agency-bordereau
		    :agency-id agency-id
		    :start-date start-date
		    :end-date end-date
		    :universal-time universal-time)))
    (prog1 bordereau
      (setf (gethash (cons agency-id universal-time) *agency-bordereaux-hash*)
	    bordereau
	    (agency-bordereau-thread bordereau)
	    (make-agency-bordereau-thread bordereau)))))

(defun agency-bordereau/post (agency-id
			                        start-date
			                        end-date
			                        spreadsheet-type
			                        universal-time)
  (with-user ()
    (let* ((bordereau (gethash (cons agency-id universal-time)
			                         *agency-bordereaux-hash*))
	         (bordereau (if (null bordereau)
			                    (new-agency-bordereau
			                     agency-id
			                     start-date
			                     end-date
			                     universal-time)
			                    bordereau)))
      (cond ((eql :done (agency-bordereau-status bordereau))
	           (<> '(ecm/ui/page:page :title "Agency Bordereau Report : ECM"
		               :refresh "1")
	             (setf (agency-bordereau-status bordereau)
		                 :exported)))
	          ((eql :exported (agency-bordereau-status bordereau))
	           (with-output-to-string (sexpml:*sexpml-output*)
	             (<download-spreadsheet>
		            (agency-bordereau-pathname bordereau)
		            :type spreadsheet-type)))
	          (t
	           (<> '(ecm/ui/page:page :title "Agency Bordereau Report : ECM"
		               :refresh "5")
	             (<> :text "Running Agency Bordereau... refreshing in 5 seconds"
		             #+(or)bordereau)))))))

(hunchentoot:define-easy-handler (agency-report-post/handler
                                  :uri "/ecm/report/agency-bordereau/post")
    (agency-id
     start-date
     end-date
     spreadsheet-type
     universal-time)
  (agency-bordereau/post agency-id
			 start-date
			 end-date
			 spreadsheet-type
			 universal-time))

(hunchentoot:define-easy-handler (agency-report-handler
                                  :uri "/ecm/report/agency-bordereau")
    (agency-id)
  (<> '(ecm/ui/page:page :title "Agency Bordereau")
    ;; (<> 'ecm/ui/navbar:navbar)
	(<> '(div :class "row row-centered report")
	  (<> 'html5:h1 "Agency Bordereau")
	  (<> 'br))
	(<agency-bordereau-report-form> agency-id)
	        (<> 'style
          (<> "iframe {
    border: none;
    resize: both;
  overflow: auto;
}"))
        (<> '(iframe :name "report"
              :id "report"
              :onload "this.width=$(\".container\").width();
                       var h = $(window).height() - $('#report').offset().top - 5;
                         $('#report').height(h);"
              :style " overflow: scroll;")
          " ")))

(defun <agency-bordereau-report-form> (agency-id)
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
  (<> '(form :method "GET"
	:action "/ecm/report/agency-bordereau/post"
	:target "report")
    (<> `(input :name "universal-time"
	  :value ,(get-universal-time)
	  :type "hidden"))

    (<> '(div :class "row row-centered")
      (<> '(div :class "col-md-8 col-centered")
        (<> '(div :class "row-centered")

	  ;;
          (<> 'h3 (<> ""))
	  (ecm/user:with-user ()
	    (<> '(select :name "agency-id")
	      (dolist (s (list-agencies))
		(<> `(option :value ,(second s)
			     ,@(when (equalp (princ-to-string (second s))
					     agency-id)
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
                          "option" "dateFormat" "yy-mm-dd"))
               (let* ((date (ps:new (|Date|)))
                      (month (ps:chain date (get-month)))
		      (end-date (ps:new (|Date|))))
		 (|.| date (set-month
                            (if (= month 0)
                                11
                                (ps:decf month))))
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
          (<> 'h3 (<> "End Date (excluding)"))
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
	 :in-browser nil
	 :name "spreadsheet-type"
	 :class "spreadsheet-type-select")))

    (<> '(input :name "present[SPREADSHEET-TYPE]"
	  :value "Gnumeric_Excel:xlsx2"
	  :type "hidden"))))

(defun list-agencies ()
  (postmodern:query "
SELECT *
 FROM (SELECT person_name(person) AS name, person_id
       FROM person
       WHERE person_id IN (SELECT DISTINCT agency_id FROM contract))
 AS c
 ORDER BY name"))
