(in-package :maxclaims)

(defclass history-component ()
  ((start-date :input (:type simple-date)
	       :accessor start-date
	       :validate (boundp))
   (end-date :input (:type simple-date)
	     :accessor end-date
	     :validate (boundp)))
  (:metaclass described-component-class)
  (:attributes start-date end-date))

(define-description history-search-result ()
  ())

(define-description link-to-history-object ()
  ())

(define-display
  :in-description link-to-history-object
  :around
  ((desc t) display object)
  (if (layer-active-p (defining-description 'html-description))
      (with-inactive-descriptions (link-to-history-object)
	(<ucw:a :action (view-object object)
		"View Object"))
      (call-next-method)))


(define-description history (description-for-history)
  ((active-attributes :value '(modification-time object-type object-id
			       (object :activate (link-to-history-object)))))
  (:in-description history-search-result))

(defcomponent history-search-results (search-results)
  ())

(defmethod render-search-controls :after ((self history-search-results))
  (<ucw:submit :action (answer nil) :value "Exit"))

(defaction search-link-action ((self history-search-results) record)
  (view-object record))

;;; todo: ajax
(defmethod render ((self history-search-results))
  (with-inactive-descriptions (ajax-description)
    (unless (paged-set.emptyp self)
      (let ((results (paged-set.current-page self)))
	(let ((lol:*display* self))
	  (<:table
	   (with-active-descriptions (history-search-result)	 
	     (with-described-object ((aref results 0))
	       (<:tr (dolist (a (attributes *description*))
		       (<:th (display-attribute-label a))))))
	   (loop for r across results
	      do (arnesi:rebind (r)
		   (display-search-result self r))))))
      (render-search-controls self))))

(defmethod display-search-result ((self history-search-results) (record history))
  (with-active-descriptions (history-search-result)
    (with-described-object (record)
      (<:tr
       (dolist (a (attributes *description*))
	 (with-attribute-context (a)
	   (with-active-descriptions (inline)
	     (<:td (display-attribute-value a)))))))))

#+nil(define-display
  :in-description history-search-result
  ((description history) (display history-search-results) (object history))
  (let ((history-object (attribute-value (find-attribute description
							 'object))))
    (<:tr (<:td (<ucw:a :action (view-object history-object)
			(<:format "~A ~A" (history.object-type object)
				  (history.object-id object))))
	  (<:td (<ucw:a :action (view-object object) "View")))))

(defaction search-history ((self history-component))
  (call 'history-search-results
	;; fixme: cannot do :order-by with select-objects
	:data (rofl::query-objects 'history
				   (lambda (table fields)
				     `(:order-by
				       (:select ,@fields :from ,table
					 :where (:between modification-time
							  ,(start-date self)
							  ,(simple-date:time-add
							    (end-date self)
							    (simple-date:encode-interval :day 1))))
				       'modification-time)))))

(defmethod render ((self history-component))
  (with-active-descriptions (validate)
    (<ucw:form :action  (when (validp self)
			  (search-history self))
	       (read-object self)
	       (<:submit :value "Search")
	       (<ucw:a :action (answer nil) "Cancel"))))
