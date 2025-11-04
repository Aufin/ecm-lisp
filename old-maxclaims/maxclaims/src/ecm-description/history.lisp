(defpackage :maxclaims/ecm-description/history
  (:use))
(in-package :maxclaims/ecm-description)

(defun as-<table (rows)
  (let ((des
	 (loop :for row :in rows
	    :collect (loop for (key . val) in row 
			:collect (list (make-symbol key) :value val
				       :label t)))))

    (let ((att-labels 
	   (mapcar #'maxclaims/ecm-description::attribute-label
		   (first des))))
      (<:table 
       :class "table table-striped table-bordered"
       (<:thead 
	(<:tr 
	 (dolist (label att-labels)
	   (<:th
	    (<:as-html label)))))
       (<:tbody
	(dolist (attributes des)
	 (<:tr
	  (dolist (a attributes)
	    (<:td (display (maxclaims/ecm-description:attribute-value a) :inline))))))))))
