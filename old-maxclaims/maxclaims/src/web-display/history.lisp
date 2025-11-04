(defpackage :maxclaims/web-display/history
  (:use :cl )
  (:import-from :maxclaims/log/object-history
		 #:select-object-history
		 #:select-object-previous-history)
  (:import-from :maxclaims/web-display/html-page
		#:with-user-html-page)
  (:export #:display-object-history))
(in-package :maxclaims/web-display/history)

(defun history-page (object)
  (with-user-html-page (:title "history") 
    (maxclaims::with-udb 
      (display-object-history object))))

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
	     (<:td (maxclaims/web-display/display:display 
		    (maxclaims/ecm-description:attribute-value a) 
		    :date-time))))))))))

(defun display-object-history (object)
  (when object 
    (let* ((name (string-downcase (class-name (class-of object))))
	   (pkeys (remove-if-not #'rofl::slot-definition-primary-key-p 
				 (c2mop:class-slots (class-of object))))
	   (value (ignore-errors (c2mop:slot-value-using-class 
				  (class-of object) object (first pkeys)))))
      (if value
	  (progn  (as-<table 
		   (select-object-history (substitute #\_ #\- name) 
					  value))
		  (as-<table 
		   (select-object-previous-history (substitute #\_ #\- name) 
						   value)))
	  (progn 
	    (when maxclaims::*debug-on-error*
	      (error "no link for ~A" object )))))))
