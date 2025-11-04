(defpackage :ecm/endpoint/transaction
  (:use :cl)
  (:import-from :ecm/user)
  (:import-from :ecm/entity/transaction)
  (:import-from :ecm/endpoint
		#:define-endpoint)
  (:import-from :ecm/request-context)
  (:import-from :ecm/ui/transaction
		#:<transaction-script>)
  (:import-from :ecm/json #:getjso))
(in-package :ecm/endpoint/transaction)

(define-endpoint table-inline
    "ecm/claim/(\\d+)/transaction-table")

(defun table-inline/get
    (claim-id
     &aux (claim-id (parse-integer claim-id)))
  (ecm/request-context:with-request-context ()
    (let* ((claim (ecm/entity/claim:find-claim-crux claim-id))
	         (transactions (getjso "transactions" claim)))
      (with-output-to-string (sexpml:*sexpml-output*)
	      (ecm/ui/transaction:<claim-transaction-table>  transactions
						                                           :claim-id claim-id
						                                           :script nil)))))

(define-endpoint create-inline
  "ecm/claim/(\\d+)/transaction/create/inline")

(defmacro plet* (nil &body body)
  `(let* ((date (ecm/hunchentoot:parameter-or-nil "transaction-date"))
	        (type (ecm/hunchentoot:parameter-or-nil "transaction-type"))
	        (heading (ecm/hunchentoot:parameter-or-nil "transaction-heading"))
	        (expense-type (ecm/hunchentoot:parameter-or-nil "transaction-expense-type"))
	        (corpus-id (ecm/hunchentoot:parameter-or-nil
		                  "corpus-id" :identity #'parse-integer))
          (recipient-id (ecm/hunchentoot:parameter-or-nil
		                  "recipient-id" :identity #'parse-integer))
	        (amount (ecm/hunchentoot:parameter-or-nil "transaction-amount"))
	        (limit-of-cover (ecm/hunchentoot:parameter-or-nil "transaction-limit-of-cover"))
	        (approved (ecm/hunchentoot:parameter-or-nil "transaction-approved"))
	        (cheque-number (ecm/hunchentoot:parameter-or-nil "transaction-cheque-number"))
	        (reference-number (ecm/hunchentoot:parameter-or-nil "transaction-reference-number"))
	        (schemes-advance-number (ecm/hunchentoot:parameter-or-nil "transaction-schemes-advance-number"))
          (first-name (ecm/hunchentoot:parameter-or-nil "first-name"))
	        (last-name (ecm/hunchentoot:parameter-or-nil "last-name"))
	        (company-name (ecm/hunchentoot:parameter-or-nil "company-name"))
	        (birth-date (ecm/hunchentoot:parameter-or-nil "birth-date"))
	        (address-line-1 (ecm/hunchentoot:parameter-or-nil "address-line-1"))
	        (address-line-2 (ecm/hunchentoot:parameter-or-nil "address-line-2"))
	        (city (ecm/hunchentoot:parameter-or-nil "city"))
	        (province-id (ecm/hunchentoot:parameter-or-nil "province-id"))
	        (postal-code (ecm/hunchentoot:parameter-or-nil "postal-code"))
	        (email-address (ecm/hunchentoot:parameter-or-nil "email-address"))
	        (home-phone (ecm/hunchentoot:parameter-or-nil "home-phone"))
	        (work-phone (ecm/hunchentoot:parameter-or-nil "work-phone"))
	        (fax (ecm/hunchentoot:parameter-or-nil "fax"))
	        (cell-phone (ecm/hunchentoot:parameter-or-nil "cell-phone"))
	        (r-first-name (ecm/hunchentoot:parameter-or-nil "r-first-name"))
	        (r-last-name (ecm/hunchentoot:parameter-or-nil "r-last-name"))
	        (r-company-name (ecm/hunchentoot:parameter-or-nil "r-company-name"))
	        (r-birth-date (ecm/hunchentoot:parameter-or-nil "r-birth-date"))
	        (r-address-line-1 (ecm/hunchentoot:parameter-or-nil "r-address-line-1"))
	        (r-address-line-2 (ecm/hunchentoot:parameter-or-nil "r-address-line-2"))
	        (r-city (ecm/hunchentoot:parameter-or-nil "r-city"))
	        (r-province-id (ecm/hunchentoot:parameter-or-nil "r-province-id"))
	        (r-postal-code (ecm/hunchentoot:parameter-or-nil "r-postal-code"))
	        (r-email-address (ecm/hunchentoot:parameter-or-nil "r-email-address"))
	        (r-home-phone (ecm/hunchentoot:parameter-or-nil "r-home-phone"))
	        (r-work-phone (ecm/hunchentoot:parameter-or-nil "r-work-phone"))
	        (r-fax (ecm/hunchentoot:parameter-or-nil "r-fax"))
	        (r-cell-phone (ecm/hunchentoot:parameter-or-nil "r-cell-phone"))
          (payee (if corpus-id
		                 (ecm/entity/corpus:find-corpus corpus-id)
		                 (when (or first-name last-name company-name)
		                   (let ((corpus (ecm/entity/corpus:create-corpus
				                              :first-name first-name
				                              :last-name last-name
				                              :company-name company-name
				                              :birth-date birth-date
				                              :address-line-1 address-line-1
				                              :address-line-2 address-line-2
				                              :city city
				                              :province-id province-id
				                              :postal-code postal-code
				                              :email-address email-address
				                              :home-phone home-phone
				                              :work-phone work-phone
				                              :fax fax
				                              :cell-phone cell-phone)))
			                   (setf corpus-id (getjso "_id" corpus))
			                   corpus))))
	        (recipient (if recipient-id
		                     (ecm/entity/corpus:find-corpus recipient-id)
		                 (when (or r-first-name r-last-name r-company-name)
		                   (let ((corpus (ecm/entity/corpus:create-corpus
				                              :first-name r-first-name
				                              :last-name r-last-name
				                              :company-name r-company-name
				                              :birth-date r-birth-date
				                              :address-line-1 r-address-line-1
				                              :address-line-2 r-address-line-2
				                              :city r-city
				                              :province-id r-province-id
				                              :postal-code r-postal-code
				                              :email-address r-email-address
				                              :home-phone r-home-phone
				                              :work-phone r-work-phone
				                              :fax r-fax
				                              :cell-phone r-cell-phone)))
			                   (setf recipient-id (getjso "_id" corpus))
			                   corpus)))))

     ,@body))


(defun create-inline/get
    (claim-id
     &aux (claim-id (parse-integer claim-id)))
  (ecm/request-context:with-request-context ()
    (ecm/ui/transaction:create-transaction-page claim-id :inline t)))

(defun create-inline/post
    (claim-id
     &aux (claim-id (parse-integer claim-id)))
  (ecm/request-context:with-request-context ()
    (plet* ()
      (flet ((transaction (&key (error nil))
	             (ecm/ui/transaction:create-transaction-page
		            claim-id :inline t :error error
		                     :date date
		                     :type type
		                     :heading heading
		                     :expense-type expense-type
		                     :approved approved
		                     :amount amount
		                     :limit-of-cover limit-of-cover
		                     :cheque-number cheque-number
		                     :reference-number reference-number
		                     :schemes-advance-number schemes-advance-number
		                     :payee payee
                         :recipient recipient)))
	      (handler-case
	          (progn (let ((transaction
			                     (ecm/entity/transaction:create-transaction
			                      claim-id
			                      :date date
			                      :type type
			                      :heading heading
			                      :expense-type expense-type
			                      :approved approved
			                      :amount amount
			                      :limit-of-cover limit-of-cover
			                      :cheque-number (or cheque-number "")
			                      :reference-number (or reference-number :null)
			                      :schemes-advance-number (or schemes-advance-number :null)
			                      :payee-id corpus-id
                            :recipient-id recipient-id)))
		                 (declare (ignorable transaction))
		                 (transaction :error transaction)
		                 (ecm/ui/transaction:edit-transaction-inline-finished
		                  (getjso "_id" transaction) (getjso "claim_id" transaction))))
			   
	      
	        (error (c) (transaction :error c)))))))
  
(define-endpoint edit-inline
    "ecm/transaction/(\\d+)/edit/inline")

(defun edit-inline/get
    (transaction-id
     &aux (transaction-id (parse-integer transaction-id)))
  (ecm/request-context:with-request-context ()
    (let ((transaction (ecm/entity/transaction:find-transaction
			transaction-id)))
      (ecm/ui/transaction:edit-transaction-page transaction :inline t))))

(defun edit-inline/post
    (transaction-id
     &aux (transaction-id (parse-integer transaction-id)))
  (ecm/request-context:with-request-context ()
    (plet* ()
      (let ((transaction (ecm/entity/transaction:find-transaction
			                    transaction-id)))
        (flet ((transaction (&key (error nil) (transaction transaction))
	               (ecm/ui/transaction:edit-transaction-page
		              transaction :inline t :error error
		                          :date date
		                          :type type
		                          :heading heading
		                          :expense-type expense-type
		                          :approved approved
		                          :amount amount
		                          :limit-of-cover limit-of-cover
		                          :cheque-number cheque-number
		                          :reference-number reference-number
		                          :schemes-advance-number schemes-advance-number
		                          :payee payee
                              :recipient recipient
                              )))
	        (handler-case
	            (progn (let ((transaction
			                       (ecm/entity/transaction:update-transaction
			                        transaction-id
			                        :date date
			                        :type type
			                        :heading heading
			                        :expense-type expense-type
			                        :approved approved
			                        :amount amount
			                        :limit-of-cover limit-of-cover
			                        :cheque-number (or cheque-number "")
			                        :reference-number (or reference-number :null)
			                        :schemes-advance-number (or schemes-advance-number :null)
			                        :payee-id corpus-id
                              :recipient-id recipient-id )))
		                   (declare (ignorable transaction))
		                   (ecm/ui/transaction:edit-transaction-inline-finished
		                    transaction-id (getjso "claim_id" transaction))))
			   
	      
	          (error (c) (transaction :error c))))))))
  
(define-endpoint toggle-approved
    "ecm/transaction/(\\d+)/toggle-approved")

(defun toggle-approved/get (transaction-id
			    &aux (transaction-id (parse-integer transaction-id)))
  (ecm/request-context:with-request-context ()
    (unless (ecm/user:user-is-administrator-p)
      (error "User must be an Adminstrator"))
    (let ((approved (ecm/entity/transaction:toggle-approved transaction-id)))
      (ecm/request-context:send-json-response
       (ecm/json:jso "transaction_id" transaction-id
		     "approved" (ecm/json:as-json-bool approved))))))
