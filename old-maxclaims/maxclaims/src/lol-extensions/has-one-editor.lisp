(in-package #:maxclaims)

;; has-one (table <- foreign-key) relation editor

;; A bit ugly because ROFL does not have a distinction between
;; one-to-many and one-to-one relations (yet)

(deftype has-one () 'list)

(defclass has-one-attribute-editor (attribute-editor)
  ())


(defun update-has-one (object attribute foreign-object)
  (with-described-object (object (lol::attribute-description attribute))
    (update-foreign-object object (attribute-slot-name attribute)
			   foreign-object)))

(defun create-has-one (object attribute foreign-object)
  (with-described-object (object (lol::attribute-description attribute))
    (insert-foreign-object object (attribute-slot-name attribute)
			   foreign-object)))


(defmethod display-html-attribute-editor (attribute (editor has-one-attribute-editor))
  (let ((object (attribute-object attribute))
	(value (attribute-value attribute)))
    (cond ((not (lol::unbound-slot-value-p value))
	   (read-object (car value))
	   (<:input :type "hidden"
		    :name (register-callback
			   (lambda (val)
			     (declare (ignorable val))
			     (when (car value)
			       (update-has-one object attribute (car value)))))
		    :value nil)
	   (<ucw:submit :action (delete-foreign-object object
						       (attribute-slot-name attribute)
						       (car value))
			"Reset"))
	  
	  (t (let* ((new (make-object
			  (rofl::slot-definition-foreign-relation
			   (find (attribute-slot-name attribute) 
				 (lol::class-slots (class-of object))
				 :key (function rofl::slot-definition-name)))))
		    (bound-slots (bound-slot-names new)))
	       (read-object new)
	       (<:input :type "hidden"
		    :name (register-callback
			   (lambda (val)
			     (declare (ignorable val))
			     (when (arnesi:aand (bound-slot-names new)
						(not (equalp arnesi:it bound-slots)))
			       (create-has-one object attribute new))))
		    :value nil))))))

;; if attribute is bound
;;  read-object ...
;; unconditionally update-foreign-instance (maybe one extra db write)

;; if attribute is unbound
;;  read-object (make-object ...) (see create-new-hash-many)
;;  if bound slots have changed
;;   insert-foreign-instance
