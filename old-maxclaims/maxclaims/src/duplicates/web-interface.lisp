(in-package #:maxclaims)

(defgeneric display-merge-results (results))
(arnesi:defgeneric/cc find-all-duplicated-records* (table))
(arnesi:defgeneric/cc duplicate-search (search-type search-term))
(arnesi:defgeneric/cc duplicate-search* (search-type search-term
					 &optional selected-duplicates))
(arnesi:defgeneric/cc perform-merge (self))
(arnesi:defgeneric/cc confirm-merge (master duplicates))

(define-description duplicate-merge-description ()
  ()
  (:mixinp t)
  (:documentation "Used when displaying important parts of a record to
be used in a merge."))

;;; Initial Search Page
(defcomponent duplicate-search-component ()
  ((search-type :initarg :record-type)
   (search-term :initarg :search-term)))

(defcomponent duplicate-results-not-ready ()
  ()
 (:documentation "Placeholder to page for results until they are ready"))

(defcomponent duplicate-search-results (search-results)
  ()
 (:metaclass described-lisp-on-lines-component-class))

(defmethod render ((self duplicate-search-component))
  (<:h1 "Duplicate Record Search")
  (unless (and (slot-boundp self 'search-type)
	       (slot-boundp self 'search-term))
      (<ucw:form :action (duplicate-search self)
	(<:table
	 (<:tr (<:td (<ucw:select 
		      :writer (lambda (s) (setf (slot-value self 'search-type) s))
		      (<ucw:option :value 'contract "Contract")
		      (<ucw:option :value 'person "Person/Company")
		      (<ucw:option :value 'policy "Policy")))
	       (<:td (<ucw:submit :action (answer (get-search-result
						   (find-all-duplicated-records*
						    (slot-value self 'search-type))
						   :display-using 'duplicate-search-results))
				  "List All Records with Duplicates")))
	 (<:tr (<:td
		(<ucw:input :writer (lambda (s) (setf (slot-value self 'search-term)
						 s))
			    :style "width: 95%"))
	       (<:td	 
		(<ucw:submit
		 :action (answer (duplicate-search (slot-value self 'search-type)
						   (slot-value self 'search-term)))
		 "Manual Search")))))))

(defaction duplicate-search (search-type search-term)
  (get-search-result (funcall 
		      (ecase search-type
			((contract) #'string-search-contract)
			((person) #'string-search-person)
			((policy) #'string-search-policy))
		      search-term 
		      :exact t)
		     :display-using 'duplicate-search-results))

(defaction duplicate-search* (search-type search-term &optional selected-duplicates)
  (get-search-result (remove-if (lambda (o) (member o selected-duplicates :test #'db=))
				(funcall 
				 (ecase search-type
				   ((contract) #'string-search-contract)
				   ((person) #'string-search-person)
				   ((policy) #'string-search-policy))
				 search-term
				 :exact t))
		     :display-using 'duplicate-search-results))

(defaction search-link-action ((self duplicate-search-results) result)
  (when-bind record (call 'duplicate-object-selector :object result)
    (answer record)))

(defaction find-all-duplicated-records* (table)
  #+nil(break "f*")
  (multiple-value-bind (records found?)
      (find-all-duplicated-records table :wait nil)
    #+nil(break "f? ~A r ~A " found? records)
    (if found?
	records
	(call 'duplicate-results-not-ready))))

(defmethod render ((self duplicate-results-not-ready))
  (<:script
   (<:as-html
    (js:js (*yahoo*.lang.later (* 5 1000)
			       window
			       (lambda (ignored)
				 (window.location.reload false))
			       nil
			       nil))))
  (<:h1 "Please Wait While Search Results are Found")
  (<:p "This page will reload every five seconds until results the are
  ready. Be advised that this may take several minutes."))

;;; Duplicate record viewer
(defcomponent duplicate-object-selector (object-viewer-component)
  ())

(defmethod render-actions :before ((self duplicate-object-selector))
  (<:ul (<:li :style "list-style-type:none;"
	      (<ucw:a :action (answer (object self))
		      "Select Record for Merging"))))

;; fixme: this is a horrible hack, really need to rebind
;; active-attributes or something instead
(defmethod render-actions :after ((self duplicate-object-selector))
  (<:h2 "Likely Duplicates")
  (if-bind likely-duplicates (find-likely-duplicates (object self))
    (<:ul
       (dolist* (likely-duplicate likely-duplicates)
	 (<:li (<ucw:a :action (when-bind dup (call 'duplicate-object-selector
						    :object likely-duplicate)
				 (answer dup))
		       (display-inline likely-duplicate)))))
      (<:p "None Found")))

;;; Main merge UI
(defcomponent duplicate-merge-component ()
  ((master-instance :accessor master-instance :initarg :master)
   (duplicates :accessor duplicates :initform (list))))

(defcomponent confirm-merge-component ()
  ((master :initarg :master)
   (duplicates :initarg :duplicates)))

(defmethod render ((self duplicate-merge-component))
  (when (slot-boundp self 'master-instance)
    (<:h1 "Merge Duplicates")
    (<ucw:form
     :action (refresh-component self)
     (<:p (<ucw:submit :action (let ((r (perform-merge self)))
				 (when r
				   (answer
				    (call 'merge-results
					  :results r
					  :master (master-instance self)))))
		       "Perform Merge")))
    
    (<:table
     (<:tr
      (<:td :style "vertical-align: top"
       (<:h2 "Master Instance")
       (<:p (with-active-descriptions (link-to-viewer inline)
	      (display self (master-instance self))))
       (display self (master-instance self)
		:activate '(duplicate-merge-description))
    
       (<:h2 "Duplicates to be Merged")
       (if-bind duplicates (duplicates self)
	 (<:ul (dolist (record duplicates)
		 (<:li (with-active-descriptions (link-to-viewer inline)
			 (display self record))
		       (<:ah " ")
		       (<ucw:a :action (setf (duplicates self)
					     (remove record (duplicates self)))
			       "(remove)"))))
	 (<:p "None Found")))
      (<:td :style "vertical-align: top"
       (<:h2 "Remaining Potential Duplicates")
       (if-bind likely-duplicates (find-likely-duplicates (master-instance self)
							  (duplicates self))
	 (<ucw:form
	  :action (refresh-component self)
	  (<:table
	   (dolist* (likely-duplicate likely-duplicates)
	     (let ((writer (lambda (v)
			     (declare (ignore v))
			     (pushnew likely-duplicate (duplicates self)
				      :test #'db=))))
	       (<:tr (<:td (<ucw:input :type "checkbox" :reader t
				       :writer writer))
		     (<:td
		      (<ucw:a :action (when-bind dup (call 'duplicate-object-selector
							   :object likely-duplicate)
					(pushnew dup (duplicates self) :test #'db=))
			      (display-inline likely-duplicate))))))
	   (<:tr (<:td :colspan "2" (<:submit :value "Select for Merging")))))
	 (<:p "None Found"))
       (<:p
	(let ((search-term nil))
	  (<ucw:form  :action (when-bind dup
				  (duplicate-search* (rofl::class-table-name
						      (class-of (master-instance self)))
						     search-term
						     (cons (master-instance self)
							   (duplicates self)))
				(pushnew dup (duplicates self) :test #'db=))
		      (<ucw:input :accessor search-term)
		      (<:ah " ")
		      (<:submit :value "Manual Search")))))))))

(defaction perform-merge ((self duplicate-merge-component))
  (let ((master (master-instance self)))
    (when (confirm-merge master (duplicates self))
      (prog1 (mapcar (lambda (record)
		       #+nil(break "merging ~A" record)
		       (multiple-value-list
			(merge-duplicate-records :from record
						 :into master
						 :user $app-user)))
		     (duplicates self))
	(delete-records-from-cache (duplicated-cache-entry-key
				    (rofl::class-table-name
				     (class-of (master-instance self))))
				   (cons (master-instance self)
					 (duplicates self)))))))

(defaction confirm-merge (master duplicates)
  (call 'confirm-merge-component
	:master master
	:duplicates duplicates))

(defmethod render ((self confirm-merge-component))
  (with-slots (master duplicates)
      self
    (<:h1 "Master Instance")
    (<:p (with-active-descriptions (inline link-to-viewer)
	   (display self master)))
    (with-active-descriptions (duplicate-merge-description)
      (display self master))
    (<:h1 "Duplicates to be Merged")
    (<:ul (dolist (duplicate duplicates)
	    (<:li (with-active-descriptions (link-to-viewer)
		    (display-inline duplicate)))))
    (<:h1 "The following records will be modified")
    (display-merge-results (mappend #'find-records-to-merge duplicates))
    (<:h1 "Perform Merge?")
    (<ucw:a :action (answer t) "Yes")
    (<:as-html " / ")
    (<ucw:a :action (answer nil) "No")))

;;; Merge Results
(defcomponent merge-results (lol-component)
  ((results :initarg :results)
   (master :initarg :master))
  (:metaclass described-component-class))

(defmethod display-merge-results (results)
  (dolist (changed-table results)
    (destructuring-bind (name key &rest ids)
	changed-table
      (when ids
	(<:h2 (<:format "~{~:(~A~) ~}" (split-sequence #\- (symbol-name name))))
	(<:ul (dolist (obj (select-objects name :where `(:in ,key (:set ,@ids))))
		(<:li (with-active-descriptions (inline link-to-viewer)
			(display *current-component* obj)))))))))

;; ((merge (results ...)) ...)
(defmethod render ((self merge-results))
  (<:h1 "The Following Records Have Been Updated")
  (<ucw:form :action (refresh-component self)
   (<:p
    (<ucw:submit :action (answer (mapcar (compose #'unmerge-duplicate-record #'car)
					 (slot-value self 'results)))
		  "Undo")))
  (dolist* ((merge results) (slot-value self 'results))
    (with-active-descriptions (duplicate-merge-description)
      (display-using-description (description-of (slot-value self 'master))
				 self
				 (merge.deleted-record merge)))
    (display-merge-results results))
  (<:p (<ucw:a :action (answer (slot-value self 'results))
	       "Done")))

;;; Ancillary Descriptions
(define-description person ()
  ((active-attributes :value '((first-name :active :when)
			       (last-name :active :when)
			       (company-name :active :when)
			       (email-address :active :when)
			       (policies-as-insured :active :when)
			       (contracts-as-agency :active :when))))
  (:in-description duplicate-merge-description))

(define-description contract ()
  ((active-attributes :value '((effective-date :active t)
			       (contract-number :active t)
			       (agency :active t))))
  (:in-description duplicate-merge-description))