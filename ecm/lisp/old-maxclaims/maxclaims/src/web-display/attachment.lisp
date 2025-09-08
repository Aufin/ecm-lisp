(defpackage :maxclaims/web-display/attachment
  (:use :cl :maxclaims/yaclml/tags)
  (:import-from :maxclaims/web-display/display
		#:activate-lambda))

(in-package :maxclaims/web-display/attachment)

(defmethod activate-lambda 
    ((name (eql 'maxclaims/ecm-description:download-attachment)))
  (lambda (o f)
    (declare (ignore f))
    (<:a :href
	 (format 
	  nil "download?claim=~A&attachment=~A"
	  (maxclaims::attachment.claim-id 
	   (slot-value o 'maxclaims/ecm-description::attachment))
	  (maxclaims::attachment.attachment-id (slot-value o 'maxclaims/ecm-description::attachment))
	  )
	 (<:as-html "Download")))
  )
