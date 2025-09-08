(in-package #:maxclaims)

;;; Risk details editor

(defmethod attribute-editor ((attribute risk-details-attribute))
  (make-instance 'risk-details-attribute-editor))

(defmethod attribute-object-risk-type ((attribute risk-details-attribute)
				       (object risk))
  (risk.type object))

(defmethod attribute-value-detail-type ((attribute risk-details-attribute)
					(detail-detail risk-risk-detail))
  (risk-risk-detail.risk-detail-type detail-detail))

(defmethod attribute-value-detail ((attribute risk-details-attribute)
				   (detail-detail risk-risk-detail))
  (risk-risk-detail.risk-detail detail-detail))

(deftype select-risk-risk-detail-detail ()
  `(or (cons risk-risk-detail-detail) null))

(defclass select-risk-risk-detail-detail-attribute-editor (attribute-editor)
  ()
  (:default-initargs :type 'select-risk-risk-detail-detail))

(defmethod display-html-attribute-editor (attribute
					  (editor select-risk-risk-detail-detail-attribute-editor))
  #+nil(break "~A" (attribute-value attribute))
  (let ((detail (let ((val (attribute-value attribute)))
		  (unless (lol::unbound-slot-value-p val)
		    (car val))))
	(risk-risk-detail (attribute-object attribute)))
    (display *current-component*
	     (or detail
		 (make-instance 'risk-risk-detail-detail
				:risk-risk-detail risk-risk-detail))
	     :activate '(editing-details)
	     ;; change to risk itself and handle finding
	     ;; risk-risk-detail in notify-...
	     :within (attribute-value (find-attribute (current-description)
						      'containing-object))
	     ;:within risk-risk-detail
	     )))

(define-display :in-description editing-details ((desc risk-risk-detail-detail))
  (let ((rrd-attribute (find-attribute desc 'risk-risk-detail))
	(r-desc-attribute (find-attribute desc 'detail)))
    (when-bind ddt (ignore-errors
		     ;; todo: be a bit less blunt (capture unbound slot error?)
		     (risk-detail.detail-detail-type
		      (risk-risk-detail.risk-detail
		       (attribute-value rrd-attribute))))
      (<:format "~A " ddt)
      (let ((reader (dlambda (a)
		      (let ((val (attribute-value a)))
			;; fixme: `lol:attribute-editor' has an
			;; `unbound-value' slot that probably should
			;; be used here and in
			;; `html-description-value' &c
			(if (lol::unbound-slot-value-p val)
			    ""
			    val))))
	    (writer (dlambda (v) (setf (attribute-value r-desc-attribute) v))))
	(<ucw:input :reader (funcall reader r-desc-attribute)
		    :writer writer)))))

(define-description risk-risk-detail (foo-foo-detail)
  ((active-attributes :value '((risk-detail :input (:type select-relation-detail)
				:label nil)
			       (detail :input (:type select-risk-risk-detail-detail)
				:active :when))))
  (:in-description editing-details))

(define-description risk-risk-detail-detail (description-for-risk-risk-detail-detail)
  ((containing-object :keyword :within)
   (active-attribute :value '(detail)))
  (:in-description editing-details))

(define-layered-method (setf lol::attribute-value-using-object)
  :in-layer #.(defining-description 'editing-details)
  :around
  (value (object risk-risk-detail-detail) attribute)
  (let ((old-value (attribute-value attribute)))
    (call-next-layered-method)
    (notify-detail-changed (attribute-value (find-attribute (current-description)
							    'containing-object))
			   object
			   (unless (lol::unbound-slot-value-p old-value)
			     old-value)
			   value)))

(defmethod notify-detail-changed ((risk risk)
				  (rrdd risk-risk-detail-detail)
				  old-value
				  new-value)
  #+nil(break "O: ~S / N: ~S" old-value new-value)
  (let ((risk-risk-detail (find (risk-risk-detail-detail.risk-risk-detail rrdd)
				(risk.details risk)
				:test #'recklessly-db=)))
    (notify-detail-changed risk-risk-detail
			   rrdd
			   old-value
			   new-value)
    ;; in theory, this will be called after the risk has been inserted
    (when risk-risk-detail
      ;; fixme: this is a bit hackish--the risk-risk-detail *may* have
      ;; been unset, but the risk-risk-detail-detail instance will
      ;; continue to exist with no associated risk-risk-detail until
      ;; the database operations are actually performed
      (update-foreign-object risk 'details risk-risk-detail))))

(defmethod notify-detail-changed ((object risk-risk-detail)
				  (rrdd risk-risk-detail-detail)
				  (old-value null)
				  (new-value string))
  (insert-foreign-object object 'detail rrdd))

(defmethod notify-detail-changed ((object risk-risk-detail)
				  (rrdd risk-risk-detail-detail)
				  (old-value string)
				  (new-value null))
  (delete-foreign-object object 'detail rrdd))

(defmethod notify-detail-changed ((object risk-risk-detail)
				  (rrdd risk-risk-detail-detail)
				  (old-value string)
				  (new-value string))
  (update-foreign-object object 'detail rrdd))
