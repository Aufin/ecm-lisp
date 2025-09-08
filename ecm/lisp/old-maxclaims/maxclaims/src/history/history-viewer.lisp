(in-package :maxclaims)

(defcomponent object-history-component (object-viewer-component)
  ((object :accessor object 
	   :initarg :object))
  (:metaclass described-component-class))



(defun display-history-table (component rows)
  (lol::funcall-with-described-object 
   (lambda ()
     (let ((attributes (with-active-descriptions (inline)
			 (attributes *description*)))
	   (*display* component))
       (<:table 
       (<:thead 
	(<:tr 		    
	 (when rows 
	   (dolist (a attributes)
	     (<:th (display-attribute-label a))))))  
       
       (when rows
	 (flet ((display-rows (rows)
		  (dolist (r rows)
		    (with-described-object (r nil)
		      (<:tr (dolist (a attributes)
			      (with-attribute-context (a)
				(<:td (with-active-descriptions (inline)
					(display-attribute-value a))))))))))
	   (display-rows rows))))))
   (first rows) nil))

(defmethod render-object-viewer ((self object-history-component) object)
  (<:h3 "History:")
  (let ((history (select-objects 
			'history.hstore-history 
			:where
			`(:and (:= row-id 
				   ,(slot-value object 
						(rofl::class-id-column-name 
						 (class-of object))))
			       (:ilike row-type 
				       ,(s-sql:sql-escape 
					  (rofl::class-table-name (class-of object))))))))
    (if history
	(display-history-table self history)
	(<:as-html "No History Available")))
  (<:hr)
  (<:as-html (object-id object))
  (<ucw:form :action (answer nil) 
	     (<:submit :value "Back")))