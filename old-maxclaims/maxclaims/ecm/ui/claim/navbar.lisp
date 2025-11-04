(defpackage :ecm/ui/claim/navbar
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.|)
  (:import-from :ecm/api/find)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/ui/risk
		#:<risk>)
  (:import-from :ecm/ui/tabs)
  (:import-from :ecm/ui/iframe)
  (:import-from :ecm/ui/persistent-header)
  (:import-from :ecm/local-time)
  (:import-from :ecm/entity/ui)
  (:import-from :ecm/ui/corpus
		#:corpus-name)
  (:import-from :ecm/endpoint/attachment)
  (:import-from :ecm/ui/utility #:cat)
  (:import-from :ecm/ui/claim #:<claim-navbar>)
  (:import-from :ecm/user
		#:user #:user-id)
  (:import-from :ecm/json
		#:getjso)
  (:import-from :ecm/ui/navbar)
  (:export #:<claim-navbar>))
(in-package  :ecm/ui/claim/navbar)

(defun values-claim/insured/status (claim-number insured status
				    &key (style  "color : white"))
  (flet ((claim ()
	   (<> '(large :style "display: inline-block; color:white;")
	     (<> '(html5:small :class "text-muted")
	       (<> :unescaped "Claim&nbsp;#"))
	       (<> :text claim-number)))
	       (insured ()
           (if (eql insured :null)
               (<> :text "NO INSURED")

	             (<> (large :class "nav-insured d-block d-lg-inline-block")
	               (<> '(html5:small :class "text-muted d-none d-md-inline-block") "Insured")
	               (<> :unescaped "&nbsp;")
	               (<> (html5:a :href (concatenate
				                             'string "/ecm/view?person="
				                             (princ-to-string (getjso "_id" insured)))
			                        :style style
			                        :target "_blank"
			                        :title (corpus-name insured))
		               (<> :text (corpus-name insured))))))
	   (status ()
	     (<> '(large :style "display: inline-block; color:white;")
	       (<> '(html5:small :class "text-muted")
		 (<> :unescaped "&nbsp;" "status" "&nbsp;"))
	       (<> :text status))))
    (Values #'claim #'insured #'status)))


(defun <claim-navbar> (claim-number insured status)
  (<> (ecm/ui/navbar:navbar
       :type "claim"
       :id claim-number)
    (multiple-value-bind (c i s)
	      (values-claim/insured/status claim-number insured status)
      (<> (span :id "navClaimTitle" :class "navbar-text")
        (<> (span)
	        (funcall c)
	        (<> :unescaped "&nbsp;")
	        (funcall s))
	      (<> :unescaped "&nbsp;")
	      (funcall i)))))
