(defpackage :ecm/ui/utility
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/entity/corpus)
  (:import-from :ecm/json
		#:getjso)
  (:import-from :ecm/local-time)
  (:import-from :simple-date)
  
  (:export #:cat
	   #:format-timestring
	   #:corpus-name
	   #:values-claim/insured/status
	   #:<link-to-viewer>
	   #:<item>
	   #:go-back-to-claim
	   #:location->string))
(in-package :ecm/ui/utility)

(setf (fdefinition 'corpus-name)
      (fdefinition 'ecm/entity/corpus:corpus-name-as-string ))

(defun cat (&rest things)
  (apply #'concatenate 'string
	 (loop :for thing :in things
	    :collect (typecase thing
		       (sequence thing)
		       (t (princ-to-string thing))))))

(defun go-back-to-claim (claim-number
			 &key (text "Go Back To Claim #") (tag "h1")
			   (style ""))

  (setf claim-number (princ-to-string claim-number))
  (when claim-number 
    (<> (div 
	 :style style
	 :class "back-to-claim")
      (<> 'style ".icon-x2{
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
      #ecmBody {
        min-height: 100%;
        height: auto !important;
        height: 100%;
        /* Negative indent footer by it's height */
        margin: 0 auto -60px;
      }
      
")
	(<> 'br)
	(<> (a 
	     :href (concatenate 'string "/ecm/view?claim=" (string claim-number))
	     :rel "prev"
	     :title (concatenate 'string "< Go back to claim #" (string claim-number)))

	  (<> :unescaped
	   "<"tag">"
	   '#:| &nbsp;<i class="fa fa-chevron-left" aria-hidden="true"></i>| (or text "") (string claim-number) "</"tag">")))))

(defmacro <link-to-viewer> ((type id &key style)
			    &body body)
  (let ((idn (gensym)))
    
    `(let ((,idn ,id))
       (<> (a
	    :href (cat "/ecm/view?" ,type "="
		       ,idn)
	:target "_blank"
	,@(when style `(:style ,style)))
     ,@body))))

(defun <item> (label data &key style unescaped)
  (let ((s style))
    (when data
      (<> '(span :style "display:inline-block")
	;; Label
	(<> '(html5:small :class "text-muted")
	  (let ((label (with-output-to-string (s)
			 (map nil (lambda (c)
				    (if (char= #\Space c)
					(write-string "&nbsp;" s)
					(write-char c s)))
			      label))))
	    (<> :unescaped label " ")))
	;; Data
	(<> '(span :style "display:inline-block")
	(typecase data
	  (ecm/json:jso
	   (cond ((equal "corpus_summary" (getjso "_type" data))
		  (<link-to-viewer>
		      ("person" (getjso "_id" data)
				:style s)
		  
		    (<> :text (corpus-name data))))
		 (t (if unescaped
			(<> :unescaped data)
			(<> :text data)))))
	   (t (if unescaped
			(<> :unescaped data)
			(<> :text data)))))))))

(defun format-timestring (time &key (format ecm/local-time:+rfc-1123-format+)
				 (timezone (ecm/user:user-local-time-timezone)))
  (flet ((fmt (local-time &optional (timezone timezone))
	   (ecm/local-time:format-timestring 
	    nil
	    local-time
	    :format format
	    :timezone timezone)))
    (handler-case
	(typecase time
	  (string 
	   (if (alpha-char-p (aref time 0))
	       time
	       (fmt (ecm/local-time:parse-rfc3339-timestring time))))
	  (simple-date:timestamp
	   (let* ((string (simple-date:timestamp-to-universal-time time))
		  (string (ecm/local-time:universal-to-timestamp string)))
	     (fmt string)))
	  (simple-date:date
	   (multiple-value-bind (y m d)
	       (simple-date:decode-date time)
	     (let* ((local-time (ecm/local-time:encode-timestamp 0 0 0 0 d m y)))
	       (fmt local-time))))
	  (ecm/local-time:timestamp (fmt time))
	  (t (princ-to-string time)))
      (error (c)
	(format nil "ERROR for ~W: ~A" time c)))) )

(defun values-claim/insured/status (claim-number insured status
				    &key (style  "color : white"))
  (flet ((claim ()
	   (<> '(html5:h3 :style "display: inline-block")
	     (<> '(html5:small :class "text-muted")
	       (<> :unescaped "Claim&nbsp;#"))
	       (<> :text claim-number)))
	   (insured ()
	     (<> (h3 :style "display: inline-block")
	       (<> '(html5:small :class "text-muted") "Insured")
	       (<> :unescaped "&nbsp;")
	       (<> (html5:a :href (concatenate
				   'string "/ecm/view?person="
				   (princ-to-string (getjso "_id" insured)))
			    :style style
			    :target "_blank"
			    :title (corpus-name insured))
		 (<> :text (corpus-name insured)))))
	   (status ()
	     (<> '(html5:h3 :style "display: inline-block")
	       (<> '(html5:small :class "text-muted")
		 (<> :unescaped "&nbsp;" "status" "&nbsp;"))
	       (<> :text status))))
    (Values #'claim #'insured #'status)))

(defun location->string (location)
  (let ((line1 (getjso "address_line_1" location))
	(line2 (getjso "address_line_2" location))
	(city (getjso "city" location))
	(province (getjso "province" location))
	(postal-code (getjso "postal_code" location))
	(country (getjso "country" location)))	
    (with-output-to-string (s)
      (when line1 (princ line1 s))
      (when (and line1 line2) (princ #\Newline s))
      (when line2 (princ line2 s))
      (when (and (or line1 line2) (or city province postal-code)) (princ #\Newline s))
      (when city (princ city s))
      (when (and city province) (princ ", " s))
      (when province (princ province s))
      (when (or city province) (princ " " s))
      (when postal-code (princ postal-code s))      
      (when (or line1 line2 city province postal-code)
	(princ #\Newline s))
      (when country (princ country s)))))
