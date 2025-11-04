(defpackage :maxclaims/ecm-description/claim/attachment
  (:use :cl)
  (:import-from :maxclaims/ecm-description
		#:define-descriptions
		#:object-attribute-value
		#:download-attachment
		#:view-link
		#:view-a-btn
		#:link-to-viewer
		#:claim 
		#:claim-id
		#:file-type
		#:sha1-digest
		#:attachment
		#:file
		#:file-name
		#:file-description)
  (:import-from #:maxclaims 
		#:filter-objects
		#:query-objects
		#:claim.claim-id))

(in-package :maxclaims/ecm-description/claim/attachment)

(defun claim-attachments (claim
			  &key (limit 1000)
			    (offset 0))
  (filter-objects 
   (query-objects 
    'attachment
    (lambda (n fs)
      `(:limit 
	(:order-by 
	 (:select ,@fs :from ,n
		  :where (:= claim-id ,(claim.claim-id claim)))
	 date)
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset)))))))

(defmethod object-attribute-value ((claim claim) 
				   (a (eql 'attachments)) 
				   &key (limit 1000)
				     (offset 0))
  (claim-attachments claim
		     :limit limit 
		     :offset offset))

(define-descriptions attachment
  (:heading 
   (file-name 
    :activate ((h2 :style "display:inline-block")))
   (claim
    :label nil
    :activate ((h4 :style "display:inline-block")
	       link-to-viewer)))
  (:default)
  (:edit (file-description 
	  :label t
	  :textarea (:rows 7 :cols 30)
	  :active t))
  (:create-heading 
   (:value :value "Create Attachment"
	   :activate (h1)))
  (:create  
   (file :input (:type "file")
	 :label "File to Attach")
   (claim-id :label t 
	     :edit nil
	     :parse parse-integer)    
   (file-description 
    :label t
    :textarea (:rows 7 :cols 30)
    :active t)
   (maxclaims::date :label t
		    :type simple-date:date
		    :edit nil)
   (file-name :label t
	      :edit nil
	      :active t)
   (sha1-digest :edit nil
		:label t
		:active :when)
   (file-type :edit nil
	      :label t
	      :active :when))
  (:timecard 
   (file-name :label t) 
   (file-description :label t
		     :active :when)
   (maxclaims::date :label t))
  (:view     
   (claim :label t) 
   (file-name :label t) 
   (file-description :label t)
   (maxclaims::date :label t)  
   (file :label "Download"
	 :activate (download-attachment)))
  (:view-tab)
  (:inline
    (view-a-btn
     :label (:create attachment
		     claim-id)
     :activate (view-link))
   (file-name :label t) 
   (file-description :label t)
   (maxclaims::date :label t)  
   (file :label "Download"
	 :activate (download-attachment))))

(defclass attachment-download ()
  ((attachment :initarg attachment)))

(defmethod object-attribute-value ((att attachment) (a (eql 'file))
				   &rest args)
  (declare (ignore args))
  (make-instance 'attachment-download 'attachment att))

(define-descriptions attachment-download
  (:default)
  (:inline identity))
