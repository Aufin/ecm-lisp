(in-package :maxclaims)

(define-layered-method lol::attribute-editp 
  :in-layer #.(defining-description 'editable)
  :around 
  ((attribute lol::standard-attribute))
  (let ((edit?       (call-next-method))
	(object (attribute-object attribute)))
    (if (and (eql edit? :admin)
	     (persistentp object))
	(or (app-user.admin $app-user)
	    (let* ((tn (s-sql:sql-escape 
			(rofl::class-table-name (class-of object))))
		   (id (object-id object)))
	      (and 
	       (cadr (select-only 1 `(:history.user-can-update ,tn ,id)))
	       t)))
	edit?)))

(defclass described-db-access-class (described-class standard-db-access-class)
  ())

;;; todo: move into lol
(defclass described-lisp-on-lines-component-class (described-class lisp-on-lines-component-class)
  ())

(define-layered-class standard-attribute (lol::define-description-attribute) ())

(defcomponent yes-or-no-p-dialog ()
  ((string :initarg :string)))

(defmethod render ((self yes-or-no-p-dialog))
  (<:h1 (<:as-html (slot-value self 'string)))
  (<ucw:a :action (answer t) "Yes")
  (<:as-html " / ")
  (<ucw:a :action (answer nil) "No"))

(defaction yes-or-no-p-dialog (string)
  (call 'yes-or-no-p-dialog :string string))

(defaction info-message-alert (text)
  (call 'ucw::info-message :message text))

(defparameter *edited-object* nil)

(defun read-object (object &rest args)
  (with-udb 
    (let ((*edited-object* (append *edited-object* (list object))))
      (with-active-descriptions (editable)
	(apply #'display *current-component* object args)))))

(defun display-inline (object &rest args)
  (with-active-descriptions (inline )
    (apply #'display *current-component* object args)))

(defun display-inline-attribute (attribute value)
  (if (ignore-errors (lol::attribute-active-attributes attribute))
      (handler-case (display-inline value :attributes (lol::attribute-active-attributes attribute))
	(error ()
	  (display-inline value)))
      (display-inline value)))

(defun display-list (list &rest args)
  (<:ul
   (arnesi:dolist* (item list)
		   (<:li (apply #'display-inline item args)))))

(defun newline-><br/> (string)
  (cl-ppcre:regex-replace (string #\Newline) string "<br/>"))

;; * FILE attribute + File Upload.

(defcomponent file-download-window (ucw-standard::window-component)
  ((stream :accessor file-input-stream :initarg :file-stream)
   (name :accessor file-name :initarg :name :initform t)))

(defmethod render :before ((component file-download-window))
  (setf (get-header (context.response *context*) "Content-Disposition")
	(format nil "attachment; filename=\"~A\"" (ignore-errors (file-name component)))))

(defmethod render ((component file-download-window))
  (let ((res (context.response *context*)))
    (unless (ucw-core::headers-are-sent-p res)
      (ucw-core::send-headers res))
    (copy-stream (file-input-stream component) (network-stream (context.response *context*)))))

(defaction download-file-stream (&key file-stream name content-type)
  (call-as-window 'file-download-window :file-stream file-stream :name name :content-type content-type))

(defcomponent simple-file-download-window (file-download-window)
  ((file :Accessor file :initarg :file)))

(defmethod render ((self simple-file-download-window))
  (with-input-from-file (stream (file self) :element-type '(unsigned-byte 8))
    (unwind-protect 
	 (progn (setf (file-input-stream self) stream)
		(call-next-method))
      (setf (file-input-stream self) nil))))

(defaction download-file (&key file name content-type)
  (call-as-window 'simple-file-download-window :file file :name name :content-type content-type))


(define-layered-class file-attribute (standard-attribute) ())

(defmethod attribute-editor ((attribute file-attribute))
  (make-instance 'file-attribute-editor))

(define-layered-method display-attribute-value 
  :in-layer #.(defining-description 'html-description)
  :around ((attribute file-attribute))
  (let ((object (attribute-object attribute))
	(value (attribute-value attribute)))
			 
    (if (layer-active-p (defining-description 'editable))
	(lol::display-attribute-editor attribute)
	(<ucw:a :action (download-file  
			 :file value 
			 :name (attachment.file-name object) 
			 :content-type (attachment.file-type object))
		"Download File"))))
  
(define-layered-class file-attribute-editor (attribute-editor))

(defmethod display-html-attribute-editor (attribute (editor file-attribute-editor))
  (let* ((object (attribute-object attribute))
	 (maker)
	 (callback (register-callback (lambda (val) (funcall maker val)))))
    (setf maker (dlambda (val)
		  ;; using content-length seems saner here, but it is
		  ;; not ever set for one reason or another.
		  (when (and (rfc2388-binary:mime-part-p val)
			     ;; unhelpfully enough file-name is ""
			     ;; instead of NIL when none is provided
			     (not (string=
				   ""
				   (rfc2388-binary:get-header-attribute 
				    (rfc2388-binary:get-header val "Content-Disposition")
				    "filename"))))
		    (setf (attachment.file-name object)
			  (rfc2388-binary:get-header-attribute 
			   (rfc2388-binary:get-header val "Content-Disposition")
			   "filename")
			  
			  (attachment.file-type object)
			  (rfc2388-binary:content-type val)

			  (attachment.sha1-digest object)
			  (ironclad:byte-array-to-hex-string
			   (ironclad:digest-file :sha1 (truename (mime-part-body val)))))
		    
		    (with-output-to-file (stream (ensure-directories-exist (attribute-value attribute)) 
						 :if-does-not-exist :create
						 :element-type '(unsigned-byte 8))
		      (copy-stream (mime-part-body val) stream)))))      
    (<:input :type "file" :name callback)))

(define-layered-class file-slot-attribute (file-attribute slot-definition-attribute) ())


;; * HTML tag attribute : Wrap a tag around the attribute value

(define-layered-class html-tag-attribute ()
  ((html-tag :initform "pre" :Accessor attribute-html-tag)))

(define-layered-method 
    lol::display-html-attribute-value :around (object (attribute html-tag-attribute))
    (call-next-method))

(define-layered-method 
    lol::display-attribute-value :around ((attribute html-tag-attribute))
    (let ((tag (Attribute-html-tag attribute)))
      (format yaclml:*yaclml-stream* "<~A>" tag)
      (call-next-method)
      (format yaclml:*yaclml-stream* "</~A>" tag)))

(define-layered-class html-tag-slot-attribute (slot-definition-attribute html-tag-attribute)
  ())


;; * Boolean :input type.

(deftype bool () '(or null t))

(defclass bool-attribute-editor (attribute-editor) ())

(defmethod display-html-attribute-editor (attribute (editor bool-attribute-editor))
  (let* ((checked)
	 (value (attribute-value attribute))
	 (writer (make-attribute-value-writer attribute))
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
    (<:as-html value)
    (<:input :type "checkbox" 
	     :checked value 
	     :name run-if-checked-callback)
    (<:input :type "hidden"
	     :name always-run-callback)))

;; * Textarea.
(deftype textarea () '(or null  t))

(defclass textarea-attribute-editor (attribute-editor) ())

(defmethod display-html-attribute-editor (attribute (editor textarea-attribute-editor))
  (<ucw:textarea 
   :writer (make-attribute-value-writer attribute)
   :rows 10
   :cols 50
   (lol::html-attribute-value attribute)))

(deftype currency () '(or number t))

(defclass currency-attribute-editor (number-attribute-editor)
  ())

(defmethod display-html-attribute-editor (attribute (editor currency-attribute-editor))
  (<:as-html "$") 
  (<ucw:input :type "text"
	      :reader (let ((val (lol::html-attribute-value attribute)))
			(if (numberp val)
			    (format nil "~$" val)
			    val))
	      :writer (make-attribute-value-writer attribute)))




  #+nil(let ((writer (make-attribute-value-writer attribute))
	(reset (lol::capture-description 
		attribute
		(lambda ()
		  (lol::attribute-slot-makunbound attribute))))
	(value (attribute-value attribute)))


    (cond 
      ((lol::unbound-slot-value-p value)
       (<ucw:select 
	:writer (lambda (val)
		  (funcall writer (make-instance val)))
	:accessor value
	(arnesi:dolist* (val (risk-types))
	  (<ucw:option :value val (<:as-html val)))))
      ((typep value 'standard-db-access-object)
       (with-inactive-descriptions (editable)
	 (if (prmary-key-boundp value)
	     (with-active-descriptions (link-to-viewer)
	       (display-inline value :attributes '(type)))
	     (with-active-descriptions (link-to-editor)
	       (display-inline value :attributes '(type)))))
	 (<:as-html "  ")
	 (<ucw:a :action (funcall reset)
		 (<:as-html "(reset)")))))

(deftype db-object ())

(defclass db-object-attribute-editor (attribute-editor)
  ((db-type :accessor db-type :initarg :db-type)))

(defmethod display-html-attribute-editor (attribute (editor db-object-attribute-editor))
  (let ((writer (make-attribute-value-writer attribute))
	(value (attribute-value attribute)))

    (if (lol::unbound-slot-value-p value)
	(read-object (make-instance (db-type editor))))))



;; * LIST attributes
(defvar *in-list* nil)

(define-layered-class list-attribute (lol::list-attribute)
		      ())

(define-layered-method lol::display-html-attribute-value :around  (object (attribute list-attribute))
		       (if *in-list*
			   (call-next-method)
			   (let ((*in-list* t))
			     (<:td :class "lol-attribute-value"
				   (call-next-method)))))

(define-layered-class list-slot-attribute (lol::list-attribute lol::slot-definition-attribute)
		      ())

(define-layered-method lol::display-html-attribute-value :around  (object (attribute list-slot-attribute))
		       (if *in-list*
			   (call-next-method)
			   (let ((*in-list* t))
			     (<:td :class "lol-attribute-value"
				   (call-next-method)))))
			     
    
; * Simple Date
(deftype simple-date () 'simple-date:date)

(defclass simple-date-attribute-editor (attribute-editor)
  ())

(locally (declare (optimize (debug 3)))
  (define-description simple-date:date ()
    ((date-string :function (lambda (date)
			      #+old(multiple-value-bind (y m d) 
				       (simple-date:decode-date date)
				     (format nil "~A-~A-~A" y m d))
			      (print-date date))))))

(define-description simple-date:date ()
  ((active-attributes :value '(date-string)))
  (:in-description inline))

(define-layered-method description-of ((thing simple-date:date))
  (find-description 'simple-date:date))

(define-layered-method description-of ((thing simple-date:timestamp))
  (find-description 'simple-date:date))


	       
(defmethod display-html-attribute-editor (attribute (editor simple-date-attribute-editor))
  (let ((year "")
	(month "")
	(day "")
	(object (attribute-object attribute)))
    (let ((writer (make-attribute-value-writer attribute)))
      (let ((date (attribute-value attribute)))
	(when (typep date 'simple-date::date)
	  (setf (values year month day)
		(simple-date:decode-date date))))
      (<:div
       :class "date-input yui-skin-sam"
       (let ((calendar-var (js:gen-js-name :prefix "cal"))
	     (year-id (ucw::gen-id "ucw"))
	     (month-id (ucw::gen-id "ucw"))
	     (day-id (ucw::gen-id "ucw"))
	     (cal-id (ucw::gen-id "cal")))
	 
	 (<ucw:input :accessor year :size 4 :maxlength 4 :id year-id) 
	 (<:big  (<:strong "-"))
	 (<ucw:input :accessor month :size 2 :maxlength 2 :id month-id)
	 (<:big  (<:strong "-"))
	 (<ucw:input :accessor day :size 2 :maxlength 2 :id day-id)
	 (<:script
	  (<:ah
	   ;; beware, ugly ugly parenscript lies ahead
	   (js:js*
	    `(progn
	       (*maxclaims-yui-loader*.really-insert
		(array "calendar" "dom")
		(lambda ()
		  (js:defvar ,calendar-var
		    (js:new (*yahoo*.widget.*calendar
			     ,cal-id
			     (js:create
			      :navigator t))))
		  (.select-event.subscribe
		   ,calendar-var
		   (lambda (types dates cal)
		     (let ((year
			    (*yahoo*.util.*dom.get ,year-id))
			   (month
			    (*yahoo*.util.*dom.get ,month-id))
			   (day
			    (*yahoo*.util.*dom.get ,day-id)))
		       (setf year.value
			     (aref dates 0 0 0))
		       (setf month.value
			     (aref dates 0 0 1))
		       (setf day.value
			     (aref dates 0 0 2))))
		   ,calendar-var)
		  (.render ,calendar-var)))))))
	 (<:div 
	  :class "date-legend"
	  "YYYY-MM-DD")
	 (<:div :id cal-id))

       (<ucw:input :type "hidden"
		   :writer (lambda (val)
			     (declare (ignore val))
			     (unless (and (equal "" year)
					  (equal "" month)
					  (equal "" day))
			       (when (equal "" year)
				 (setf year "1900"))
			       
			       (when (equal "" month)
				 (setf month "1"))
			       (when (equal "" day)
				 (setf day "1"))
			       (let ((date 
				      (ignore-errors 
					(apply #'simple-date:encode-date 
					       (mapcar #'(lambda (x)
							   (parse-integer x :junk-allowed t))
						       (list year month day))))))

				 (if date 
				     (funcall writer 
					      date)
				     (signal (make-condition 
					      'lol::validation-condition 
					      :format-string "Invalid Date ~A ~A ~A"
					      :format-args  (list year month day)
					      :attribute attribute
					      :object object)))))))))))

; * floating point 

(deftype floating-point () '(or number t))

(defclass floating-point-attribute-editor (number-attribute-editor)
  ())

(defmethod display-html-attribute-editor 
    (attribute (editor floating-point-attribute-editor)) 
  (<ucw:input :type "text"
	      :reader (let ((val (lol::html-attribute-value attribute)))
			(if (numberp val)
			    (format nil "~F" val)
			    val))
	      :writer (make-attribute-value-writer attribute)))
