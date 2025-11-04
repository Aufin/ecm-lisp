(in-package #:maxclaims)

;;; Details editor common code

(defun recklessly-db= (dao1 dao2)
  (if (and (persistentp dao1) (persistentp dao2))
      (db= dao1 dao2)
      (eql dao1 dao2)))

(defun attribute-detail-types (attribute risk-type)
  (select-objects (attribute-detail-type attribute)
		  :where `(:= risk-type-name
			      ,(risk-type.type-name risk-type))))

(defmethod display-html-attribute-editor ((attribute details-attribute)
					  (editor details-attribute-editor))
  ;; fixme: select-using-object needs improvement and would make this
  ;; less messy
  (let ((applicable-detail-types (ignore-errors
				   ;; a bit brute force, but it does
				   ;; the job
				   (attribute-detail-types
				    attribute
				    (attribute-object-risk-type
				     attribute (attribute-object attribute)))))
	(current-details (let ((val (attribute-value attribute)))
			   (if (lol::unbound-slot-value-p val)
			       nil
			       val))))
    #+nil (break "~A / ~A" applicable-detail-types current-details)
    (dolist (dt applicable-detail-types)
      (let* ((id (arnesi:random-string))
	     (val (find dt current-details
			:key (curry #'attribute-value-detail-type
				    attribute)
			:test #'db=)))
	(<:label :for id (<:ah (detail-type.description dt)))
	(arnesi:rebind (dt)
	  (display *current-component*
		   (or val
		       (make-object-detail (attribute-object attribute)))
		   :activate '(editing-details)
		   :detail-type dt
		   :within (attribute-object attribute)))))))

;;; Notify related risk/claim

(define-layered-method (setf lol::attribute-value-using-object)
  :in-layer #.(defining-description 'editing-details)
  :around
  (value (object relation-relation-detail) attribute)
  (let ((old-value (attribute-value attribute)))
    (call-next-layered-method)
    (notify-detail-changed (attribute-value (find-attribute (current-description)
							    'containing-object))
			   object
			   (unless (lol::unbound-slot-value-p old-value)
			     old-value)
			   value)))

;;; Detail selector for relation-relation-detail

(deftype select-relation-detail ()
  `(or relation-detail null))

(defclass select-relation-detail-attribute-editor (attribute-editor)
  ()
  (:default-initargs :type 'select-relation-detail))

(defmethod display-html-attribute-editor (attribute
					  (editor select-relation-detail-attribute-editor))
  (let ((writer (dlambda (v)
		  (setf (attribute-value attribute) v))))
    (<ucw:select :reader (attribute-value attribute)
		 :writer writer
		 :test #'db=
		 (<ucw:option :value nil (<:ah "None"))
		 (dolist (detail (detail-type.details
				  (attribute-value (find-attribute (current-description)
								   'detail-type))))
		   (arnesi:rebind (detail)
		     (<ucw:option :value detail (<:ah (detail.description detail))))))))

;;; General notification of Changes

(defmethod notify-detail-changed ((object relation-with-details-mixin)
				  (detail-detail relation-relation-detail)
				  (old-value relation-detail)
				  (new-value relation-detail))
  (unless (eq old-value new-value)
    (if (find-object-detail detail-detail object #'relation.details)
	(progn #+nil(break "updating")
	       (update-foreign-object object 'details detail-detail))
	(error "cannot do detail->detail on uninserted instance"))))

(defmethod notify-detail-changed ((object relation-with-details-mixin)
				  (detail-detail relation-relation-detail)
				  (old-value null)
				  (new-value relation-detail))
  (unless (eq old-value new-value)
    (if (find-object-detail detail-detail object #'relation.details)
	(error "cannot insert already inserted instance")
	(progn #+nil(break "inserting")
	       (insert-foreign-object object 'details detail-detail)))))

(defmethod notify-detail-changed ((object relation-with-details-mixin)
				  (detail-detail relation-relation-detail)
				  (old-value relation-detail)
				  (new-value null))
  (unless (eq old-value new-value)
    (if (find-object-detail detail-detail object #'relation.details)
	(progn #+nil(break "deleting")
	       (delete-foreign-object object 'details detail-detail))
	(error "cannot delete uninserted instance"))))
