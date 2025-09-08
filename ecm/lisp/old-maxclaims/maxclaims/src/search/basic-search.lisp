(in-package #:maxclaims)

;;; Core search components

(defaction search-link-action (self val)
  (view-object val))

(defcomponent search-results (paged-set)
  ((exact :accessor search-results-exact 
	  :initform nil
	  :initarg :exact)
   (search-term :accessor search-results-term
		:initarg :term)
   (display-exact-checkbox 
    :accessor display-exact-checkbox
    :initarg :display-exact-checkbox
    :initform t))
  (:metaclass described-lisp-on-lines-component-class))

(defgeneric render-search-link (self object)
  (:method (self object)
    (<ucw:a 
     :action (search-link-action self object) 
     (apply #'display self object
	    (when-bind a (search-result-attributes self object)
	      (list :attributes a))))))

(defaction search-link-action ((self search-results) val)
  (view-object val))

(defcomponent duplicate-record-search-results (search-results)
  ()
  (:default-initargs :display-exact-checkbox nil)
  (:metaclass described-lisp-on-lines-component-class))

(defmethod render :around ((self duplicate-record-search-results))
  (with-inactive-descriptions (editable)
    (call-next-method)))
(defmethod render-search-link 
    :around ((self duplicate-record-search-results) object)
      (break "Layers in search:~A " (contextl:active-layers))
  (<ucw:a 
   :action (answer object)
   (<:as-html "(select record) "))
  (call-next-method))

(defcomponent cheque-register-search-results (search-results)
  ()
  (:metaclass described-lisp-on-lines-component-class))

(defaction search-link-action ((self cheque-register-search-results) val)
  (view-cheque-register-report val))

(defcomponent report-search-results (search-results)
  ()
  (:metaclass described-lisp-on-lines-component-class))

(defaction search-link-action ((self report-search-results) val)
  (view-bordereau-report val))

(defcomponent report-excel-search-results (search-results)
  ()
  (:metaclass described-lisp-on-lines-component-class))

(defaction search-link-action ((self report-excel-search-results) val)
  (view-bordereau-report/excel val))

(defcomponent report/heading-search-results (search-results)
  ()
  (:metaclass described-lisp-on-lines-component-class))

(defaction search-link-action ((self report/heading-search-results) val)
  (view-bordereau-report/heading val))

(defcomponent contract-bordereau-report/heading-search-results (report/heading-search-results)
  ()
  (:metaclass described-lisp-on-lines-component-class))

(defaction search-link-action ((self report/heading-search-results) val)
  (view-contract-bordereau-report val))

(defgeneric search-result-attributes (self result)
  (:method (self result) nil))

(defgeneric render-search-controls (self))

(define-display ((desc search-results) (self search-results) displayed-object)
  (declare (ignorable displayed-object))
  (unless (paged-set.emptyp self)
    (let ((results (paged-set.current-page self)))
      (<:ul (loop for r across results 
	       do (arnesi:rebind (r)
		    (with-active-descriptions (inline)
		      (<:li
		       (<:as-html (class-name (class-of r)))
		       (<:as-html " ")
		       (render-search-link self r)
		       ))))))
    (render-search-controls self)))

(defmethod render ((self search-results))
  (display self self))

(defmethod render-search-controls :around ((self search-results))
  (call-next-method))

(defmethod render-search-controls ((self search-results))
  (<ucw:form 
   :action (refresh-component self)	       
  (let ((page-offset (paged-set.offset self)))
    (<:p (<:format "Page ~D of ~D"
		   (1+ (paged-set.offset self))
		   (paged-set.page-count self)))
    (<ucw:submit :action (scroll-forward self) "Next")
    (<ucw:submit :action (scroll-backward self) "Previous")
    (<ucw:input :reader (1+ page-offset)
		:writer (lambda (v)
			  (when-bind off
			      ;; just stay on the current page when
			      ;; out of range
			      (when-bind o (ignore-errors
					     (1- (parse-integer v)))
				(and (>= o 0)
				     (< o (paged-set.page-count
					   self))
				     o))
			    (setf page-offset off)))
		:size 3)
    (<ucw:submit :action (scroll-to-page self page-offset)
		 "Jump to Page")))
  (when (display-exact-checkbox self)
    (<ucw:form 
     :action (find-search-result 
	      self (search-results-term self)
	      :exact (search-results-exact self))
     (<:div 
      :style "border:thin solid black;"
      (let* ((checked)
	     (value (search-results-exact self))
	     (writer (lambda (v)
		       (setf (search-results-exact self) v)))
	     (always-run-callback
	       (ucw-core::register-callback  
		(lambda (ignore)
		  (declare (ignore ignore))
		  (if checked 
		      (unless value (funcall writer t)) 
		      (when value  (funcall writer nil))))))
	     (run-if-checked-callback 
	       (ucw-core::register-callback  
		(lambda (ignore)
		  (declare (ignore ignore))
		  (setf checked t)
		  ))))
	(<:input :type "checkbox" 
		 :checked value 
		 :name run-if-checked-callback)
	(<:input :type "hidden"
		 :name always-run-callback)
	(<:as-html  "\"" (search-results-term self) 
		    "\" " )
	(<:as-html (if value 
		       "E"
		       "Ine")
		   "xact Search ("
		   (unless value "not ")
		   "checked)"))
   
      (<:submit :value "Search Again")))))

(defmethod (setf search-results-exact) :around (val self)
  (when (string= val "value")
    (setf val t))
  (call-next-method val self))

(defaction get-search-result (results &key 
				      ((:display-using display-component) 'search-results)
				      term)
  (call display-component 
	:data results
	:term term))
  
(defaction display-search (self search-term)
  (call-component self (make-instance 'search-results 
     :data (search-records search-term))))


(defaction find-search-result (self search-term &key (exact 'unset))
  (when (not (equal "" search-term)) 
    (let ((claim (string-search-claim search-term :exact t)))
      (let ((results 
	      (or claim		  
		  (let ((e 
			  (remove nil (search-records search-term :exact t))))
		    (remove-duplicate-records 
		     (append 
		      (when (or e exact)
			(prog1 e
			  (when (eql exact 'unset) 
			    (setf exact t))))
		      (unless (if  e
				   (eql exact t)
				   e)
			(remove nil (search-records search-term :exact nil)))))))))
	(app-user-log :log-type (if results "SEARCH" "SEARCH UNSUCCESSFUL")
		      :log-info (format nil "results for ~A : (~A) " 
					search-term (length results)))
      (when (eql exact 'unset)
	(setf exact nil))
      (if (not (rest results))
	  (car results)
	  (call 'search-results 
		:term search-term
		:data results
		:exact exact))))))
