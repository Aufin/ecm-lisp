(in-package :maxclaims)

(define-description claim-transaction-heading (description-for-claim-transaction-heading)
  ((claim-transaction-heading-name :active :t :label nil)
   (active-attributes :value '(claim-transaction-heading-name))))

(define-description claim-transaction-heading (description-for-claim-transaction-heading)
  ((active-attributes :value '(claim-transaction-heading-name)))
  (:in-description inline))

(define-description claim-transaction-type (description-for-claim-transaction-type)
  ((description :active :t :label nil)
   (active-attributes :value '(description))))

(define-description claim-transaction-type (description-for-claim-transaction-type)
  ((active-attributes :value '(description)))
  (:in-description inline))

(define-description claim-transaction-expense-type (description-for-claim-transaction-expense-type)
  ((claim-transaction-expense-type-name :active t :label nil)
   (active-attributes :value '(claim-transaction-expense-type-name))))

(define-description claim-transaction-expense-type (description-for-claim-transaction-expense-type)
  ((claim-transaction-expense-type-name :active t :label nil)
   (active-attributes :value '(claim-transaction-expense-type-name)))
  (:in-description inline))

(define-layered-class claim-transaction-cheque-number-edit 
 (slot-definition-attribute)
  ((editable :initarg :editable 
	     :initform nil
	     :accessor claim-cheque-number-editable)))

(defun claim-cheque-number-editable-p (attribute user)
  (and (claim-cheque-number-editable attribute)
       (user-can-edit-cheque-number-p user)))

#+nil(define-layered-method lol::display-html-attribute-value
  
   (object 
    (a claim-transaction-cheque-number-edit))

  (macrolet ((a ()
	       '(progn (call-next-method)
		 (when (claim-cheque-number-editable-p a $app-user)
		 (<ucw:a 
		  :action (edit-object-attributes (attribute-object a) 
						  '(cheque-number))
		  :style "float:right;"
		  (<:as-html (format nil "~A ~A ~A" a
				     (attribute-value a)
				     'foo))
		  (<:as-html "(edit)"))))))
    (<:as-html "Now 'here")
   (if (not (claim-cheque-number-editable-p a $app-user))
       (a)
   (<:div :style "border:1px solid #BBBBBB"
	  (<:&nbsp)
	  (a)))))


(define-layered-method display-attribute-value
  :in-layer #.(defining-description 'html-description)
   ((a claim-transaction-cheque-number-edit))
  (macrolet ((a ()
	       '(progn (call-next-method)
		 (when (claim-cheque-number-editable-p a $app-user)
		 (<ucw:a 
		  :action (edit-object-attributes (attribute-object a) 
						  '(cheque-number))
		  :style "float:right;"
		  (<:as-html "(edit)"))))))
   (if (not (claim-cheque-number-editable-p a $app-user))
       (a)
   (<:div :style "border:1px solid #BBBBBB"
	  (<:&nbsp)
	  (a)))))

(lol::register-validator 'claim-transaction-amount-validator
  (lambda (a v)
    (let* ((ctd (current-description))
	   (ctt (attribute-value (find-attribute
				  ctd
				  'transaction-type)))
	   (heading 
	     (and (slot-boundp (attribute-object a) 'transaction-heading-name) 
		  (claim-transaction.transaction-heading-name 					
		   (attribute-object a)))))
      ;; maybe fixme: hardcoded name 
      (cond 
	((and
	  (not (lol::unbound-slot-value-p ctt))
	  (string= "Open Reserve"
		   (claim-transaction-type.description ctt))
	  (or (lol::unbound-slot-value-p v)
	      (<= v 0)))
	 (prog1 nil
	   (signal (make-condition
		    'lol::validation-condition
		    :format-string "Open Reserve must be greater than $0.00"
		    :format-args nil
		    :attribute a
		    :object (attribute-object a)))))
	     
	((and
	  (not (lol::unbound-slot-value-p ctt))
	  (let ((tt (claim-transaction-type.claim-transaction-type-id ctt)))
	    (or (= 3 tt)
		(= 4 tt)
		(= 5 tt)))
	  (not (lol::unbound-slot-value-p v))
	  (let* ((claim-id (claim.claim-id 
			    (claim-transaction.claim 
			     (attribute-object a))))
		 (previous (and (slot-boundp (attribute-object a) 
					     'transaction-id)
				(claim-transaction.transaction-id 
				 (attribute-object a))))
		 (reserve (claim-info-by-heading claim-id))
		 (outstanding (or (second (assoc heading reserve :test #'string-equal))
				  0))
		 (final (+ outstanding 
			   (if previous
			       (or 
				(let ((n
					(query (:select 
						'amount 
					    :from 'claim-transaction 
					    :where (:= 'transaction-id previous))
					   :single)))
				  (if (numberp n)
				      n
				      0)))
			       0))))
	    (< (- final v) 0)))
	 (prog1 v
	   (signal (make-condition
		    'lol::validation-condition
		    :format-string "Cheque must have a positive reserve for ~A"
		    :format-args (list (or heading "heading"))
		    :attribute a
		    :object (attribute-object a)))))
	(t t)))))
      

(lol::register-validator 'claim-transaction-payee-validator
			 (lambda (a v)
			   (let* ((ctd (current-description))
				  (ctt (attribute-value (find-attribute
							 ctd
							 'transaction-type))))
			     (if (and
				  (not (lol::unbound-slot-value-p ctt))
				  (let ((tt (claim-transaction-type.claim-transaction-type-id ctt)))
				    (or (= 3 tt)
					(= 4 tt)
					(= 5 tt)))
				  (lol::unbound-slot-value-p v))
				     
				 (prog1 nil
				   (signal (make-condition
					    'lol::validation-condition
					    :format-string "Cheque must include a payee"
					    :format-args nil
					    :attribute a
					    :object (attribute-object a))))
				 t))))
 

(define-description select-claim-transaction-heading (prepopulated-select)
  ())

(define-layered-method display-select-db-object-value
    :in #.(defining-description 'select-claim-transaction-heading)
    :around 
    (value editor writer attribute selected-value reset)
    #+nil(break "type : ~A" 
		(risk.risk-type-name
		 (claim.risk (claim-transaction.claim (attribute-object attribute)))))
    (if (layer-active-p (defining-description 'editable))
	(let ((fn (attribute-value (find-attribute  'select-claim-transaction-heading
				    'select-fn))))
     (with-inactive-descriptions (select-claim-transaction-heading)
       (display-select-db-object-value
	(cons (make-instance (db-type editor))
	      (funcall fn
			editor))
	editor writer attribute selected-value reset)))
     (call-next-layered-method)))

(define-description claim-transaction (description-for-claim-transaction)
  ((transaction-type  :input (:type select-db-object
				    :db-type claim-transaction-type
				    :size 1)
		      :activate (prepopulated-select)
		      :deactivate (writeable-select)
		      :validate (boundp))
   (transaction-heading :input (:type select-db-object
				      :db-type claim-transaction-heading
				      :size 1)
			:activate (select-claim-transaction-heading)
			:deactivate (writeable-select)
			:validate (boundp))
   (expense-type :input (:type select-db-object
			       :db-type claim-transaction-expense-type
			       :size 1)
		 :deactivate (writeable-select)
		 :activate (prepopulated-select))
   (payee :input (:type select-db-object
			:db-type person
			:attributes (first-name last-name company-name))
	  :validate (claim-transaction-payee-validator))
   (transaction-date :input (:type simple-date)
		     :validate (boundp))
   (amount :value-formatter $-formatter
	   :input (:type currency)
	   :editp :admin
	   :validate (claim-transaction-amount-validator))
   (active-attributes
    :value '(transaction-type
	     transaction-heading
	     (cheque-number 
	      :active :when
	      
	      )
	     payee
	     transaction-date
	     amount))))

(define-description claim-transaction (description-for-claim-transaction)
  ((cheque-number 
    :active :when
    :attribute-class claim-transaction-cheque-number-edit

   )   
   (active-attributes 
    :value '((transaction-date 
	      :activate (link-object-to-viewer))
	     transaction-type
	     transaction-heading
	     (cheque-number :active :when
	      )
	     amount)))
  (:in-description inline))

(define-description claim-transaction (description-for-claim-transaction)
  ((lol:active-attributes 
    :value '((transaction-date) 
	     (transaction-type)
	     (transaction-heading)
	     (expense-type)
	     amount
	     payee
	     reference-number
	     cheque-number
	     schemes-advance-number)))
  (:in-description editable))

(defun incurred-authority-exceeded-p (transaction &optional (user $app-user))
  ;; No authority set means no limit
  (when (slot-boundp transaction 'amount)
    (with-slots (claim amount transaction-type-id)
	transaction
      ;; only needs to checked if the amount affects claim_incurred--so
      ;; cheque-loss, cheque-expense, and outstanding-reserve
      (when-bind incurred-limit (and
				 ;; fixme: hardcoded type ids
				 ;; open reserve, cheque loss, cheque expense
				 (member transaction-type-id '(1 3 4))
				 (ignore-errors
				   (app-user-authority.claim-incurred-limit
				    ;; todo: rofl :set nil slots
				    (car (app-user.authority user)))))
	;; fixme: per-heading now that they are required? I think yes
	(let ((new-incurred (+ amount (query (:select (:claim-incurred (claim.claim-id claim)))
					     :single!))))
	  (if (> new-incurred incurred-limit)
	      (values t incurred-limit (- new-incurred incurred-limit))))))))


(defmethod/cc insert-object-action-using-object :around (component (transaction claim-transaction))
  (multiple-value-bind (exceeded-p limit exceeded-amount)
      (incurred-authority-exceeded-p transaction)
      (if exceeded-p
	  (if (yes-or-no-p-dialog
	       (format nil "Incurred limit ($~$) will be exceeded by
	       $~$. Have you received authorization?"
		       limit exceeded-amount))
	      (call-next-method component transaction))
	  (call-next-method component transaction))))


