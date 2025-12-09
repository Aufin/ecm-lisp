
(cl:defpackage :maxclaims/entry-point/create
  (:use :cl)
  (:import-from :maxclaims/hunchentoot
		#:define-easy-handler)
  (:import-from :maxclaims/entry-point/toplevel
		#:http-parameter-value
		#:http-parameters-as-alist
		#:http-parameters-changed-alist
		#:select-object-from-request
		#:select-objects/key-value
		#:object-typename-and-id-from-request)
  (:import-from :maxclaims/database
		 #:maxclaims-database-condition)
  (:import-from :maxclaims/ecm-description
		#:ecm-attributes
		#:attribute-type)
  (:import-from :maxclaims/web-display/html-page
		#:get-app-user
		#:with-user-html-page)
  (:import-from :maxclaims/web-display/display
		#:<view-link
		#:object-name-and-pkey-value)
  (:import-from :maxclaims
		#:with-udb
		#:call-with-app-user)

  (:import-from :maxclaims/web-display/edit
		#:edit-page
		#:select-object-for-edit-page
		#:back-to-create/edit)
  (:import-from :maxclaims/data-entity/app-user
		#:app-user-read-only-p
		#:app-user-administrator-p)
  (:export #:create-page-handler))


(in-package :maxclaims/entry-point/create)

(defun date-to-string (date)
  (if (stringp date)
      date
      (maxclaims/text-display:display date :inline)))

(defun dslot-value (thing name)
  (let ((thing (slot-value thing name)))
    (typecase thing
      ((or simple-date:date simple-date:timestamp)
       (date-to-string thing))
      (t thing))))

;;; * TODO all the redirects should simply be a part of the
;;; * REDIRECT-TO method

(defun redirect-to-time-recorded-report-url (object)
  (remove #\Space
	  (apply
	   #'concatenate 'string
	   `("spreadsheet?spreadsheet[time-recorded-report]=t"
	     "&spreadsheet[start-date]="
	     ,(dslot-value object 'maxclaims/ecm-description::start-date)
	     "&spreadsheet[end-date]="
	     ,(dslot-value object 'maxclaims/ecm-description::end-date)
	     "&spreadsheet[type]="
	     ,(dslot-value object 'maxclaims/ecm-description::spreadsheet-type)))))

(defun redirect-to-claim-open-report (object)
  (remove #\Space
	  (apply
	   #'concatenate 'string
	   `("spreadsheet?spreadsheet[claim-open-report]=t"
	     "&spreadsheet[type]="
	     ,(dslot-value object 'maxclaims/ecm-description::spreadsheet-type)))))

(defun redirect-to-interim-report (object)
  (remove #\Space
	  (apply
	   #'concatenate 'string
	   `("spreadsheet?spreadsheet[interim-report]=t"
	     "&spreadsheet[minutes]="
	     ,(princ-to-string (slot-value object 'maxclaims/ecm-description::minutes))
	     "&spreadsheet[type]="
	     ,(dslot-value object 'maxclaims/ecm-description::spreadsheet-type)))))


(defmacro with-app-user (() &body body)
  `(call-with-app-user
    (get-app-user)
    (lambda ()
      (with-udb
	,@body))))

(defun edit-page-handler
    (object
     &key
       back-create
       back
       file-attributes
       condition
       (changed (http-parameters-changed-alist "past" "future"))
       (type-change
	(let* ((search (http-parameters-as-alist "search"))
	       (value (second (first search))))
	  (if (and value
		   (not (string= "" value)))
	      search
	      (http-parameters-as-alist "type"))))
       (submit-text "Submit Create")
       (success-text "Create using these values")
       (access-type)
       (access-id)
       (create-key)
       (cancel-link-fn (lambda ()
			 (if access-type
			     (format nil "view?~A=~A&~{tab[active]=~A~}"
				     access-type access-id
				     (let ((active-tab
					    (second (assoc
						     "active-tab"
						     (http-parameters-as-alist
						      "back")
						     :test #'string=))))
				     (when (and active-tab
						(not (string-equal "nil" active-tab)))
				       (list active-tab))))
				     "view"))))
  ;;(break "~A" object)
  (if type-change
      (with-user-html-page ()

	(select-object-for-edit-page
	 object  (first (first type-change))
	 changed
	 :search (http-parameters-as-alist "search")
	 :layers :create
	 :heading? :create-heading))
      (edit-page object
		 :back back
		 :back-create back-create
		 :condition condition
		 :cancel-link (funcall cancel-link-fn)
		 :submit-text submit-text
		 :success-text success-text
		 :heading :create-heading
		 :inline-select? t
		 :layers :create
		 :changed-attributes
		 (append file-attributes
			 changed)
		 :future-attributes
		 `(,@(when (and  create-key
				 (not (string-equal "NIL" create-key))) `((,create-key ,access-id)))
		     ,@(http-parameters-as-alist "attribute")))))

(defun back-to-create (object back &key back-create)
;;    (break "~A" back)
  (let* ((back-create-string
	  (unless back-create
	    (cadr (assoc "create" back :test #'string=))))
	 (back-create
	  (or back-create
	      (rest (when back-create-string
		      (let ((*readtable* (copy-readtable))

			    (*read-eval* nil))
			(setf (readtable-case *readtable*) :preserve)
			(read-from-string back-create-string))))))
	 (create-new
	  (loop :for (name) :in back-create
	       :when (string-equal
		      "create-new"
		      (subseq (string name) 0
			      (position #\[ (string  name))))
	       :do (return (subseq (string name)
				   (1+ (position #\[ (string name)))
				   (position #\] (string name))))))
	 (back-create-inputs
	  (remove
	   nil
	   (cons (when object
		   (list
		    (format nil "attribute[~A]" create-new)
		    (cdr (object-name-and-pkey-value object))))
		 (loop
		    :for (name . value)
		    :in
		    (remove-if (lambda (acons)
				 (string-equal
				  (format nil "future[~A]" create-new)
				  (car acons)))
			       (remove NIL back-create :key #'cdr))
		    :collect (cons
			      (cl-ppcre:regex-replace
			       "future\\[" (string name) "attribute[")
			      value))))))

    (when back-create-inputs
      (flet ((|go| ()
	       (<:form
	 :method "POST"
	 :action (format nil "create?~{~{~A=~A~}~^&~}"
			 (remove-if (lambda (acons)
				      (string-equal
				       "attribute["
				       (subseq (string  (first acons))
					       0
					       10)))
				    back-create-inputs))
	 (loop :for (name val) :in back-create-inputs
	    :do (<:input :type "hidden" :name name :value val))
	 (<:input :type "submit"
		  :class "btn btn-success"
		  :value "Go Back to Create"))))

	(if object
	    (with-user-html-page (:title "Go Back?")
	      (<:h3 "Created:")
	      (maxclaims/web-display/display:display object :view)
	      (|go|))
	    (|go|))))))

(define-easy-handler
    (back-to-create-handler :uri "/ecm/back-to-create")
    ()
  (let ((back (http-parameters-as-alist "back")))
    (back-to-create nil back)))

(defun create-page-handler (&key
			      (changed
				  (http-parameters-changed-alist
				   "past" "future"))
			      (object-initargs '()))
  (catch 'create
    (handler-bind ((error (lambda (c)
			    (unless maxclaims::*debug-on-error*
			      (throw 'create
				(with-user-html-page (:title "Create ECM")
				  (maxclaims/web-display/navbar:navbar)
				  (<:div
				   :class "text-error"
				   (<:h1 (<:as-html "Error :"))
				   (<:pre
				    (<:as-html (princ-to-string c))))))))))

      (let* ((create (http-parameters-as-alist "create"))
			 (create-new (http-parameters-as-alist "create-new"))
			 (create-type (second (assoc "type" create :test #'string=)))
			 (create-key (second (assoc "key" create :test #'string=)))
			 (access (http-parameters-as-alist "access"))
			 (access-read-only (second (assoc "read-only" access :test #'string=)))
			 (access-administrator (second (assoc "administrator" access :test #'string=)))
			 (access-type (second (assoc "type" access :test #'string=)))
			 (access-id
			   (let ((foo (second (assoc "id" access :test #'string=))))
				 (unless (string-equal foo "NIL")
				   foo)))
			 (access-key (second (assoc "key" access :test #'string=)))
			 (access-object (and access-id
								 (with-app-user ()
								   (select-objects/key-value
									(find-symbol
									 (string-upcase access-type)
									 :maxclaims/ecm-description)
									(make-symbol (string-upcase (or access-key create-key)))
									(parse-integer access-id)))))
			 (back (http-parameters-as-alist "back"))
			 (back[create]
			   (when create-new
				 (http-parameters-as-alist))))

		(when (equalp create-type "timecard")
		  (hunchentoot:redirect
		   (concatenate
			'string "/ecm/claim/" access-id "/timecard/create"))
		  (throw 'create nil))

	(if (or access-object
			(and access-read-only
				 (not (app-user-read-only-p
					   (get-app-user))))
			(and access-administrator
				 (app-user-administrator-p
				  (get-app-user)))
			(and (equalp create-type "app-user-message"))
			(equalp access-type "manage-viewer")
			;; ** Is it a spreadsheet?
			(or (equalp create-type "contract-bordereau")
				(equalp create-type "spreadsheet-contract-bordereau")
				(equalp create-type "spreadsheet-white-oak-bordereau")
                (equalp create-type "spreadsheet-llyods-bordereau")
				(equalp create-type "spreadsheet-arch-bordereau")
				(equalp create-type "spreadsheet-payee-week")
				(equalp create-type "agency-cheque-register")
				(equalp create-type "contract-cheque-register")
				(equalp create-type "agency-bordereau")
				(equalp create-type "claim-bordereau")
				(equalp create-type "inter-hannover-bordereau")
				(equalp create-type "contracts-bordereau")))

	    ;; ** Now, make the object itself
	    ;; Uses  OBJECT-INITARGS

	    (let ((object
				(when create-type
				  (with-app-user ()
					(apply #'make-instance
						   (intern (string-upcase create-type)
								   :maxclaims/ecm-description)
						   object-initargs))))
			  ;; ** Bind PRESENT to the new PRESENT PARAMS
			  ;; This means that there are things that need added.

			  (present (http-parameters-as-alist "present"))
			  (file (http-parameters-as-alist "file"))
			  file-attributes)

	      ;; ** WHEN there is a BACK[CREATE]

	      (when back[create]
			(let* ((create-attribute
					 (assoc (caar create-new)
							(ecm-attributes object :create)
							:test #'string-equal))
				   (create-name (string-upcase
								 (attribute-type create-attribute))))
			  (return-from create-page-handler
				(edit-page-handler
				 (with-app-user ()
				   (apply #'make-instance
						  (intern create-name
								  :maxclaims/ecm-description)
						  nil))
				 :back back
				 :back-create (list* "create" create-name
									 back[create])
				 :file-attributes nil
				 :cancel-link-fn
				 (constantly
				  (lambda ()
					(back-to-create
					 nil back
					 :back-create (butlast back[create]))))
				 :changed nil
				 :create-key nil
				 :access-type nil
				 :access-id nil))))

	      ;; ** WHEN This is a FILE

	      (when file
			(destructuring-bind (path file-name content-type)
				(http-parameter-value
				 "future[FILE]" #'hunchentoot:post-parameter)
			  (let* ((sha1-digest
					   (ironclad:byte-array-to-hex-string
						(ironclad:digest-file
						 :sha1 path))))
				(push (list 'maxclaims/ecm-description::file-name file-name)
					  file-attributes)
				(push (list 'maxclaims/ecm-description::sha1-digest sha1-digest)
					  file-attributes)
				(push (list 'maxclaims/ecm-description::file-type
							content-type)
					  file-attributes)
				(alexandria:copy-file
				 path
				 (format nil "/tmp/~A_~A" sha1-digest file-name)
				 :if-to-exists :supersede
				 :element-type '(unsigned-byte 8)))))

	      ;; ** IF PRESENT
	      ;; Otherwise, there are now things that need added!
	      (if present
			  (catch 'present
				(handler-bind
					((cl-postgres:database-error
					   (lambda (c)
						 (unless maxclaims::*debug-on-error*
						   (throw 'present
							 (edit-page-handler object
												:file-attributes file-attributes
												:condition (maxclaims-database-condition
															c)
												:changed present)))))
					 (error (lambda (c)
							  (unless maxclaims::*debug-on-error*
								(throw 'present
								  (edit-page-handler
								   object
								   :file-attributes file-attributes
								   :condition c
								   :changed present))))))
				  (with-app-user ()
					(with-udb
					  (let ((present-attributes
							  (maxclaims/ecm-description:ecm-attributes
							   object :create
							   :attributes (mapcar #'first present))))
						;;			  (break "~A~%~A" present-attributes)
						(loop for (name . rest) in present-attributes
							  :do
								 (let* (

										(type (getf rest :type))
										(val (second (assoc name present :test #'string-equal)))
										(val1 (if (getf rest :parse)
												  (parse-integer val)
												  val))
										(ival (if type
												  (cond ((ignore-errors (string-equal val1 "%:false:%"))
														 NIL)
														((not (equalp val1 ""))
														 val1)
														(t :null)
														)
												  val1)))


								   (setf (maxclaims/ecm-description::object-attribute-value
										  object name)
										 ival))))
					  (maxclaims/ecm-description:insert-object object))

;;; * Back to previous create
					(let ((back-to-prev
							(assoc "create" back :test #'string=)))

					  (when back-to-prev

						#+ (or)
						(break "Back to previous create: ~A"
							   back-to-prev)

						(return-from create-page-handler
						  (back-to-create/edit
						   object back
						   :path (string-downcase  (string (first (read-from-string  (cadr  (assoc "create" back :test #'string=)))))))))))

				  (if (or (string-equal
						   create-type
						   "contracts-bordereau")
						  (string-equal
						   create-type
						   "contract-cheque-register"))
					  (maxclaims/hunchentoot:redirect
					   (concatenate
                        'string
                        "/ecm/reports?report["
                        create-type "]="
                        (if (string-equal
                             create-type
                             "contract-cheque-register")
                            (slot-value
                             object
                             'maxclaims/ecm-description::contract)
							(format
							 nil "~{~{~A,~}~}"
							 (mapcar (lambda (r)
									   (mapcar #'maxclaims::contract.contract-id
											   (maxclaims/ecm-description::contracts r)))
									 (slot-value
									  object
									  'maxclaims/ecm-description::contracts))))
						(if (string-equal
							 create-type
							 "contract-cheque-register")
							(format
							 nil "&report[contract]=~A"
							 (slot-value
							  object
							  'maxclaims/ecm-description::contract)))
						"&report[start-date]="
						(date-to-string (dslot-value object 'maxclaims/ecm-description::start-date))

						"&report[end-date]="
						(date-to-string (dslot-value object 'maxclaims/ecm-description::end-date))
						"&report[risk-type]="
						(slot-value object 'maxclaims/ecm-description::risk-type)))


;;; ** This is where we redirect to reports or spreadsheets

					  (maxclaims/hunchentoot:redirect
					   (concatenate
						'string "/ecm/"

						(cond

;;; *** This is the interim report

						  ((string-equal create-type "interim-report")
						   (redirect-to-interim-report object))

;;; *** This is the time-recorded-report

						  ((string-equal create-type "time-recorded-report")
						   (redirect-to-time-recorded-report-url object))

;;; *** This is the claim open report
						  ((string-equal create-type "open-claim-report")
						   (redirect-to-claim-open-report object))

;;; *** And the rest
						  ((string-equal create-type "agency-cheque-register")
						   (concatenate
							'string "reports?report[agency-cheque-register]="
							(slot-value object 'maxclaims/ecm-description::agency)
							"&report[start-date]="
							(date-to-string (slot-value object 'maxclaims/ecm-description::start-date))
							"&report[end-date]="
							(date-to-string (slot-value object 'maxclaims/ecm-description::end-date))
							"&report[risk-type]="
							(slot-value object 'maxclaims/ecm-description::risk-type))
						   )
						  ((string-equal create-type "contract-bordereau")
						   (apply
							#'concatenate 'string
							`("reports?report[contract-bordereau]="
							  ,(slot-value object 'maxclaims/ecm-description::contract)
							  "&report[start-date]="
							  ,(date-to-string (slot-value object 'maxclaims/ecm-description::start-date))
							  "&report[end-date]="
							  ,(date-to-string (slot-value object 'maxclaims/ecm-description::end-date))
							  "&report[risk-type]="
							  ,(slot-value object 'maxclaims/ecm-description::risk-type)
							  ,@(when (maxclaims/ecm-description::bordereau-report-as-excel
									   object)
								  `("&report[as-excel]=yes")))))
						  ((string-equal create-type "spreadsheet-payee-week")
						   (remove #\Space
								   (apply
									#'concatenate 'string
									`("spreadsheet?spreadsheet[payee-week]"

									  "&spreadsheet[start-date]="
									  ,(dslot-value object 'maxclaims/ecm-description::start-date)
									  "&spreadsheet[type]="
									  ,(slot-value object 'maxclaims/ecm-description::spreadsheet-type)))))
						  ((string-equal create-type "spreadsheet-contract-bordereau")
						   (remove #\Space
								   (apply
									#'concatenate 'string
									`("spreadsheet?spreadsheet[contract-bordereau]="
									  ,(slot-value object 'maxclaims/ecm-description::contract)
									  "&spreadsheet[start-date]="
									  ,(date-to-string (slot-value object 'maxclaims/ecm-description::start-date))
									  "&spreadsheet[end-date]="
									  ,(date-to-string (slot-value object 'maxclaims/ecm-description::end-date))
									  "&spreadsheet[risk-type]="
									  ,(slot-value object 'maxclaims/ecm-description::risk-type)
									  "&spreadsheet[type]="
									  ,(slot-value object 'maxclaims/ecm-description::spreadsheet-type)))))
						  ((string-equal create-type "spreadsheet-white-oak-bordereau")
						   (remove #\Space
								   (apply
									#'concatenate 'string
									`("spreadsheet?spreadsheet[white-oak-bordereau]="
									  ,(slot-value object 'maxclaims/ecm-description::contract)
									  "&spreadsheet[start-date]="
									  ,(dslot-value object 'maxclaims/ecm-description::start-date)
									  "&spreadsheet[end-date]="
									  ,(dslot-value object 'maxclaims/ecm-description::end-date)
									  "&spreadsheet[risk-type]="
									  ,(slot-value object 'maxclaims/ecm-description::risk-type)
									  "&spreadsheet[type]="
									  ,(slot-value object 'maxclaims/ecm-description::spreadsheet-type)))))
						  ((string-equal create-type "spreadsheet-llyods-bordereau")
						   (remove #\Space
								   (apply
									#'concatenate 'string
									`("spreadsheet?spreadsheet[llyods-bordereau]="
									  ,(slot-value object 'maxclaims/ecm-description::contract)
									  "&spreadsheet[start-date]="
									  ,(date-to-string (slot-value object 'maxclaims/ecm-description::start-date))
									  "&spreadsheet[end-date]="
									  ,(date-to-string (slot-value object 'maxclaims/ecm-description::end-date))
									  "&spreadsheet[risk-type]="
									  ,(slot-value object 'maxclaims/ecm-description::risk-type)
									  "&spreadsheet[type]="
									  ,(slot-value object 'maxclaims/ecm-description::spreadsheet-type)))))
						  ((string-equal create-type "spreadsheet-arch-bordereau")
						   (remove #\Space
								   (apply
									#'concatenate 'string
									`("spreadsheet?spreadsheet[arch-bordereau]="
									  ,(slot-value object 'maxclaims/ecm-description::contract)
									  "&spreadsheet[start-date]="
									  ,(date-to-string (slot-value object 'maxclaims/ecm-description::start-date))
									  "&spreadsheet[end-date]="
									  ,(date-to-string (slot-value object 'maxclaims/ecm-description::end-date))
									  "&spreadsheet[risk-type]="
									  ,(slot-value object 'maxclaims/ecm-description::risk-type)
									  "&spreadsheet[type]="
									  ,(slot-value object 'maxclaims/ecm-description::spreadsheet-type)))))
						  ((string-equal create-type "spreadsheet-inter-hannover-bordereau")
						   (remove #\Space
								   (apply
									#'concatenate 'string
									`("spreadsheet?spreadsheet[inter-hannover-bordereau]="
									  ,(slot-value object 'maxclaims/ecm-description::contract)
									  "&spreadsheet[start-date]="
									  ,(date-to-string (slot-value object 'maxclaims/ecm-description::start-date))
									  "&spreadsheet[end-date]="
									  ,(date-to-string (slot-value object 'maxclaims/ecm-description::end-date))
									  "&spreadsheet[risk-type]="
									  ,(slot-value object 'maxclaims/ecm-description::risk-type)
									  "&spreadsheet[type]="
									  ,(slot-value object 'maxclaims/ecm-description::spreadsheet-type)))))
						  ((string-equal create-type "spreadsheet-claim-bordereau")
						   (remove #\Space
								   (apply
									#'concatenate 'string
									`("spreadsheet?spreadsheet[claim-bordereau]="
									  ,(slot-value object 'maxclaims/ecm-description::contract)
									  "&spreadsheet[start-date]="
									  ,(dslot-value object 'maxclaims/ecm-description::start-date)
									  "&spreadsheet[end-date]="
									  ,(dslot-value object 'maxclaims/ecm-description::end-date)
									  "&spreadsheet[risk-type]="
									  ,(slot-value object 'maxclaims/ecm-description::risk-type)
									  "&spreadsheet[type]="
									  ,(slot-value object 'maxclaims/ecm-description::spreadsheet-type)))))
						  ((string-equal create-type "claim-bordereau")
						   (apply
							#'concatenate 'string
							`("reports?report[claim-bordereau]="
							  ,(slot-value object 'maxclaims/ecm-description::contract)
							  "&report[start-date]="
							  ,(dslot-value object 'maxclaims/ecm-description::start-date)
							  "&report[end-date]="
							  ,(dslot-value object 'maxclaims/ecm-description::end-date)
							  "&report[risk-type]="
							  ,(slot-value object 'maxclaims/ecm-description::risk-type)
							  ,@(when (maxclaims/ecm-description::bordereau-report-as-excel
									   object)
								  `("&report[as-excel]=yes")))))

						  ((string-equal create-type "inter-hannover-bordereau")
						   (apply
							#'concatenate 'string
							`("reports?report[inter-hannover-bordereau]="
							  ,(slot-value object 'maxclaims/ecm-description::contract)
							  "&report[start-date]="
							  ,(date-to-string (slot-value object 'maxclaims/ecm-description::start-date))
							  "&report[end-date]="
							  ,(date-to-string (slot-value object 'maxclaims/ecm-description::end-date))
							  "&report[risk-type]="
							  ,(slot-value object 'maxclaims/ecm-description::risk-type)
							  ,@(when (maxclaims/ecm-description::bordereau-report-as-excel
									   object)
								  `("&report[as-excel]=yes")))))
						  ((string-equal create-type "agency-bordereau")
						   (concatenate
							'string "reports?report[agency-bordereau]="
							(slot-value object 'maxclaims/ecm-description::agency)
							"&report[start-date]="
							(date-to-string (slot-value object 'maxclaims/ecm-description::start-date))
							"&report[end-date]="
							(date-to-string (slot-value object 'maxclaims/ecm-description::end-date))
							"&report[risk-type]="
							(slot-value object 'maxclaims/ecm-description::risk-type))
						   )


						  (t
						   (if access-type
							   (format nil "view?~A=~A&~{tab[active]=~A~}"
									   access-type access-id
									   (let ((active-tab
											   (second (assoc
														"active-tab"
														(http-parameters-as-alist
														 "back")
														:test #'string=))))
										 (when (and active-tab
													(not (string-equal "nil" active-tab)))
										   (list active-tab))))
							   (destructuring-bind (name . val)
								   (object-name-and-pkey-value object)
								 (format nil "view?~A=~A" name val)))))))
					  )))

			  ;; * Now there is no present
			  (let ((back-create (cadr (assoc "create" back
											  :test #'string-equal))))
				#+nil(break "back create (cadr (assoc \"create\" back :test #'string-equal)):~% ~A"
							back-create)
				(edit-page-handler object
								   :back-create
								   (when back-create
									 nil
									 (let ((*readtable* (copy-readtable))

										   (*read-eval* nil))
									   (setf (readtable-case *readtable*) :preserve)
									   (read-from-string back-create)))


								   :file-attributes file-attributes
								   :changed changed
								   :create-key create-key
								   :access-type access-type
								   :access-id access-id))))

	    (<:as-html "Nothing to create :" create-type))))))

(define-easy-handler (|create| :uri "/ecm/create")
    ()
  (create-page-handler))

(define-easy-handler (|create-report| :uri "/ecm/create-report")
    ((textarea :real-name "present[CONTRACTS-TEXTAREA]")
     (atextarea :real-name "attribute[CONTRACTS-TEXTAREA]"))
  (let ((changed (http-parameters-changed-alist "past" "future"))
	(initargs nil))
    (when (or textarea
	      atextarea)
      (let* ((contract-numbers
	      (split-sequence:split-sequence
	       #\Return
	       (remove #\Linefeed (or textarea
				      atextarea))
	       :remove-empty-subseqs t))
	     (contracts
	      (loop for cn in contract-numbers
		 :collect
		   (make-instance
		    'maxclaims/ecm-description::contracts-bordereau-contracts
		    :contract-number cn
		    :contracts
		    (let ((app-user (ignore-errors (get-app-user))))			   (if app-user
			       (call-with-app-user
				app-user
				(lambda ()
				  (maxclaims::with-adb
				    (maxclaims::string-search-contract cn :exact nil)))))))
		   )))
	(setf initargs (list :contracts contracts))
	#+ (or) (break "~A: ~A"  contract-numbers contracts) ))

    (create-page-handler  :object-initargs initargs)))
