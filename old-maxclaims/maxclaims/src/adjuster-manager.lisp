(in-package #:maxclaims)

(defcomponent adjuster-manager-component () ())

(defmethod render ((self adjuster-manager-component))
  (let* ((adjusters (select-objects 'app-adjuster)))
    (<:fieldset 
     (<:legend "Adjuster Manager")
     
     (<ucw:a :action (create-object (make-instance 'app-adjuster)) 
	     "Add New Adjuster")
     (<:fieldset 
      (<:legend "Active Adjuster List")
      (<:ul
       (dolist* (adjuster adjusters)
	 (<:li 
	  (<ucw:a :action (edit-object adjuster)
		  (display-inline adjuster))
	  (<:ah " ")
	  (<ucw:a :action (when (yes-or-no-p-dialog
				 (format nil "Remove ~S From Active  Adjusters?"
					 (display nil (app-adjuster.person adjuster)
						  :activate '(inline))))
			    (delete-object adjuster))
		  "(delete)"))))))))