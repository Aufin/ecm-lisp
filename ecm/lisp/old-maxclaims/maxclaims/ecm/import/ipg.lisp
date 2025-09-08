(defpackage :ecm/import/ipg
    (:use :cl)
    (:import-from :cl-csv))

  (in-package :ecm/import/ipg)

  (defun create-struct (name columns)
    (let ((cols (loop for a in columns 
                   :collect (with-output-to-string (s)
                              (let ((first t))
                                (map nil (lambda (char)
                                           (when (and (not first)
                                                      (upper-case-p char))
                                             (princ "-" s))
                                           (setf first nil)                                          
                                           (when (not (char= char #\space))
                                             (princ (char-downcase char) s)))
                                     a))))))
      (eval (read-from-string
             (format nil "(defstruct (~A (:type list)) ~{~A ~})"
                     name cols)))
      cols))

  (defun create-insured-s-sql (row)
    (let ((name (csv-claim-insured row))
          (address1 (csv-claim-address1 row))
          (address2 (csv-claim-address2 row))
          (city (csv-claim-city row))
          (postal-code (csv-claim-postal-code row))
          (day-phone (csv-claim-day-phone row))
          (home-phone (csv-claim-home-phone row))
          (fax (csv-claim-fax row))
          (cell-phone (csv-claim-cell-phone row)))
      `(:import-person-id
        ,name
        (:json-build-object
         "address1" ,address1
         "address2" ,address2
         "city" ,city
         "postal_code" ,postal-code)
        (:json-build-object
         "home_phone" ,home-phone
         "work_phone" ,day-phone
         "fax_phone" ,fax
         "cell_phone" ,cell-phone))))

  (defun create-company-s-sql (name &optional  (must-exist t))
    `(:import-person-id ,name :null :null ,must-exist))

  (defvar *csv-sapphire*)

  (defun csv-claim-existing-agent (row)
    (let ((agent (string-right-trim " " (csv-claim-agent row))))
      (cond ((string= agent "Trans Canada Insurance Marketi")
             "Trans Canada Insurance Marketing")
            ((string= agent "Mainguy Assurances & Services ")
             "Mainguy Assurances & Services")

            (t agent))))

  (defun csv-claim-insurer-name (row)
    (csv-sapphire-insurer-name
     (find (csv-claim-insurer row)
           *csv-sapphire*
           :key #'csv-sapphire-insurer
           :test #'string-equal)))

  (defvar *missing-sapphire* nil)

  (defun csv-claim-sapphire (row)
    (let* ((saph (find (csv-claim-claim row)
                       *csv-sapphire*
                       :key #'csv-sapphire-claim
                       :test #'string-equal)))
      (unless saph
        (warn "~A is not in sapphire!" (csv-claim-claim row))
        (pushnew (csv-claim-claim row) *missing-sapphire* :test #'equal))
      saph))

  (defun csv-claim-underwriter-name (row)
    (let* ((saph (csv-claim-sapphire row))
           (name (csv-sapphire-insurer-group-name saph)))
      (if (string= name "Lloyd's of London (Group - see ClientCompany for Insurers/Coverholders)")
          "Lloyd's of London"
          name)))


  (defun create-insert-contract-s-sql ()
    `(:select (:as (:import_contract_id
               "IPG Closed XL Catlin Import"
               (:import-person-id "Trans Canada Insurance Marketing")
               "2017-07-01" "2018-07-01")
                   id)))

  (defun create-policy-s-sql (row)
    (let ((number (csv-claim-policy row))
          (agent (csv-claim-existing-agent row))
          (company (csv-claim-insurer-name row))
          (underwriter (csv-claim-underwriter-name row)))
      `(:select (:import-policy-id
                 ,(create-insured-s-sql row)
                 ,number
                 ,(create-company-s-sql agent)
                 ,(create-company-s-sql company)
                 ,(if underwriter (create-company-s-sql underwriter) :null)
                 ,(or-null (csv-claim-policy-eff-date row))
                 ,(or-null (csv-claim-policy-exp-date row))))))


  (defun create-insert-bowood-s-sql ()
    '(:select (:import-person-id "Bowood")))

  (defun create-insert-kiln-s-sql ()
    '(:select (:import-person-id "KILN C/O BOWOOD INSURANCE BROKERS LIMITED")))

  (defun create-insert-pro-s-sql ()
    '(:select (:import-person-id "PROFESCAU ASSURANCE SPECIALISE")))

  (defun create-insert-ben-s-sql ()
    '(:select (:import-person-id "BENEFICIAL INSURANCE SOLUTIONS")))

  (defun create-insert-xl-s-sql ()
    '(:select (:import-person-id "XL Catlin Insurance")))

  (defun create-insert-faraday-s-sql ()
    '(:select (:import-person-id "Faraday Underwriting Ltd")))

  (defun create-insert-rj-s-sql ()
    '(:select (:import-person-id "RJ Kiln & co Limited")))

  (defun create-insert-mainguy-s-sql ()
    '(:select (:import-person-id "Mainguy Assurances & Services")))

  (defun create-insert-accent-s-sql ()
    '(:select (:import-person-id "Accent Solutions d'Assurances")))

  (defun create-insert-liberty-s-sql ()
    '(:select (:import-person-id "Liberty Managing Agency Ltd.")))

  (defun create-insert-bfl-s-sql ()
    '(:select (:import-person-id "BFL Canada Insurance Services")))

  (defun create-insert-lus-s-sql ()
    '(:select (:import-person-id "Lussier Dale Parizeau")))

  (defun create-import-risk-s-sql (type policy)
    `(:select (:import-risk-id
               ,type ,policy
               (:select * :from import-contract))))

  (defun csv-claim-ecm-loss-code (row)
    (let* ((code (csv-claim-loss-code row))
           (ecms '(("FIRE" . "A1")
                   ("PD" . "12")
                   ("PRLB" . "12")
                   ("HAIL" . "Z9")
                   ("SB" . "R1")
                   ("WD" . "D1")
                   ("WIND" . "B5")
                   ("MECHB" . "J2")
                   ("ACCD" . "X1")
                   ("BI" . "31")
                   ("FREEZ" . "X1")
                   ("IMP" . "K3")
                   ("PALLR" . "12")
                   ("THEFT" . "X1")
                   ("BSINT" . "W5")
                   ("COLL" . "K3")
                   ("CROP" . "G1")
                   ("COLLA" . "12")
                   ("POWER" . "99")
                   ("VAND" . "85")
                   ("LIGHT" . "10")
                   ("BE" . "22")
                   ("SNOW" . "99")
                   ("SMOKE" . "A1")
                   ("CRIME" . "99")
                   ("MD" . "99")
                   ("CGL" . "99")))
           (ecm (cdr (assoc code ecms :test #'string=))))
      :null))

  (defun csv-claim-loss-province (row)
    (csv-sapphire-loss-province
     (csv-claim-sapphire row)))

  (defun csv-claim-loss-catastrophe (row)
    (let ((c (csv-sapphire-catastrophe-desc
              (csv-claim-sapphire row))))
      (if (or (not c) (equalp c ""))
          :null
          c)))

  (defun csv-claim-line-of-business (row)
    (let ((c (csv-sapphire-business-type
              (csv-claim-sapphire row))))
      (if (or (equalp c "Farm")
              (not c) (equalp c ""))
          :null
          c)))


  (defun or-null (thing)
    (if (or (not thing)
            (equalp thing ""))
        :null
        thing))

  (defun csv-claim-ecm-status (csv-claim)
    (let ((status (csv-claim-status csv-claim)))
      (case (intern status)
        (c "Closed")
        ((:or o x) "Open")
        (t (error "Unknown status ~A ~%~A" status csv-claim)))))

  (defun create-import-claim-s-sql (risk row)
    `(:select (:import-claim-id
               ,risk
               ,(csv-claim-claim row)
               ,(csv-claim-ecm-status row)
               (:json-build-object
                "date" ,(csv-claim-loss-date row)
                "code" ,(csv-claim-ecm-loss-code row)
                "description" ,(csv-claim-loss-description row)
                "location" (:json-build-object
                            "address1" ,(csv-claim-loss-address row)
                            "city" ,(csv-claim-loss-city row)
                            "province" ,(csv-claim-loss-province row)
                            "postal_code" ,(csv-claim-loss-postal-code row))
                "catastrophe" ,(csv-claim-loss-catastrophe row))
               ,(or-null (csv-claim-received-date row))
               ,(or-null (csv-claim-setup-start row))
               ,(or-null (csv-claim-date-opened row))
               ,(or-null (csv-claim-date-reopened row))
               :null
               ,(csv-claim-line-of-business row)
               ,(or-null (csv-claim-deductible row))
               nil
               0
               :null
               :null
               :null)))



 (defparameter *sapphire-path* nil)


  (defun import-ipg-claims-sql (&key (claims (cl-fad:merge-pathnames-as-file
                                              (user-homedir-pathname)
                                              "Downloads/TCIM-Claims.csv"))
                                  (csv-claim (with-open-file (f claims)
                                               (cl-csv:read-csv f))))
    (let* ((sapphire *sapphire-path*)
           (*csv-sapphire* (with-open-file (f sapphire)
                             (cl-csv:read-csv f))))
      (create-struct 'csv-sapphire (first *csv-sapphire*))
      (create-struct 'csv-claim (first csv-claim))
      (format nil "BEGIN ; ~{~A ; ~%~} COMMIT;"
              (mapcar (lambda (s)
                        (if (stringp s)
                            s
                            (s-sql:sql-compile s)))
                      (list* (format nil "CREATE TEMPORARY TABLE import_contract AS ~A"
                                     (s-sql:sql-compile (create-insert-contract-s-sql)))
                             
                             #+(or) (progn (create-insert-kiln-s-sql)
					   (create-insert-pro-s-sql)
                             (create-insert-ben-s-sql)(create-insert-xl-s-sql)
                                           (create-insert-bowood-s-sql)
                                           (create-insert-faraday-s-sql)
                                           (create-insert-rj-s-sql)
                                           (create-insert-mainguy-s-sql)
                                           (create-insert-accent-s-sql)
                                           (create-insert-liberty-s-sql)
                                           (create-insert-bfl-s-sql)
                                           (create-insert-lus-s-sql))
                            (loop for row in (rest csv-claim)
                               :collect (create-import-claim-s-sql
                            (create-import-risk-s-sql
                             "Commercial"
                             (create-policy-s-sql row))
                            row)))))))


  (defun csv-transaction-heading (row)
    (let* ((code (csv-transaction-reserve-code row))
           (ecms '(("EXP" . "Expert Expense")
                   ("PBLDG" . "Building")
                   ("PAJEX" . "Adjusting")
                   ("PBSE" . "Building")
                   ("PBR" . "Building")
                   ("FBLDG" . "Building")
                   ("PFDPT" . "Expert Expense")
                   ("PCNTS" . "Contents")
                   ("FCNTS" . "Contents")
                   ("PCOED" . "Contents")
                   ("FOBLD" . "Contents")
                   ("FAJEX" . "Expert Expense")
                   ("PINEX" . "Expert Expense")
                  ("PTI" . "Additional Living Expense")
                  ("PXTRA" . "Additional Coverages")
                  ("FXTRA" . "Additional Coverages")
                  ("SALV" . "Additional Coverages")
                  ("PAPEX" . "Expert Expense")
                  ("FAPEX" . "Expert Expense")
                  ("PSIGN" . "Expert Expense")
                  ("PPOED" . "Indemnity")
                  ("PBINT" . "Business Interruption")
                  ("FBINT" . "Business Interruption")
                  ("PENEX" . "Expert Expense")
                  ("PCAEX" . "Expert Expense")
                  ("PBLAW" . "Legal")
                  ("PPFEX" . "Expert Expense")
                  ("PEQP" . "Equipment")
                  ("PTOOL" . "Equipment")
                  ("PRI" . "Loss of Rent")
                  ("RECO" . "Disbursement")
                   ("PPD" . "Building")
                   ("PDEDC" . "TPA" )
                   ("PDEB" . "Expert Expense")
                   ("PMP" . "Indemnity")
                   ("PSUBS" . "Investigator Fees")
                   ("PSTCK" . "Stock")
                   ("PCE" . "Equipment")
                   ("POBLD" . "Building")
                   ("PRENT" . "Loss of Rent")
                   ("LAJEX" . "TPA")
                   ("LBI" . "Medical/Rehabilitation")
                   ("PLEX" . "Legal")
                   ("PALE" . "Additional Living Expense")
                   ("PMS" . "Additional Living Expense")
                   ("LLEXP" .  "Legal")
                   ("PCRIM" . "Crime")
                   ("LDEDC" . "TPA" )
                   ("LPD" . "Building")
                   ("LAPEX" . "Expert Expense")
                   ("PCOND" . "Expert Expense")
                   ("PPA" . "Indemnity")
                   ("SUBRO" . "Disbursement")
                   ("LCAEX" . "Expert Expense")
                   ("FEQP" ."Equipment")
                   ("PLOU" . "Medical/Rehabilitation")))
           (heading (cdr (assoc code ecms :test #'string=))))
      (unless heading
        (error "No heading for reserve code ~A" code))
      heading))

  (defun create-import-transaction-s-sql (row)
    (let* ((claim (csv-transaction-claim row))
           (item-type (csv-transaction-item-type row))
           (exp (csv-transaction-explanation row))
           (heading (csv-transaction-heading row))
           (type (cond ((and (string= item-type "R")
                             (string= exp "Initial Reserve"))
                        "Open Reserve")
                       ((and (or (string= item-type "R")
                                 (string= item-type "Z"))
                             (or (string= exp "Reserve adjusted")
                                 (string= exp "Réserve redressée")
                                 (string= exp "Effacer réserve")
                                 (string-equal exp "Reversal")
                                 (string= exp "Increased reserve")
                                 (string= exp "Clear reserve")
                                 (string= exp "Billing" :end1 (length "Billing"))))
                             "Reserve Adjustment")
                       ((string= item-type "P")
                        (if (member heading '("Adjusting" "Expert Expense")
                                    :test 'string=)
                            "Cheque - Expense"
                            "Cheque - Loss"))
                       ((string= item-type "Q")
                        (if (member heading '("Adjusting" "Expert Expense")
                                    :test 'string=)
                            "Cheque - Expense"
                            "Cheque - Loss"))

                       (t (error "Cannot find proper type for type ~W : ~W"
                                 item-type exp))))
           (date (csv-transaction-item-date row))
           (amount (csv-transaction-amount row))
           (cheque-number (csv-transaction-cheque-number row))
           (payee (or-null (csv-transaction-payee row)))
           (payee-id (if (eq :null payee)
                         payee
                         (create-company-s-sql payee nil)))

           (reference-number (or-null
                              (when (string= item-type "P")
                                exp)))
           (approved (when (string= item-type "P")
                       t)))
      (when (and (ignore-errors (> 0 (read-from-string amount)))
                 (string= "Open Reserve" type))
        (return-from create-import-transaction-s-sql))

      `(:SELECT (:import-transaction-id
                 (:select claim-id
                          :from import-claim
                          :where (:= import-claim-id ,claim))
                 ,type
                 ,heading
                 ,date
                 ,amount
                 ,cheque-number
                 ,payee-id
                 ,reference-number
                 ,approved))))


 (defun import-ipg-transactions (&key
                                   (trans (cl-fad:merge-pathnames-as-file
                                              (user-homedir-pathname)
                                              "xl-catlin/jul21/TCIM Export4/External/TCIMK TCIML TCIMF-ReserveItems.csv"))
                                   (csv-transaction (with-open-file (f trans)
                                                      (cl-csv:read-csv f))))
    (let* ((headers (create-struct 'csv-transaction (first csv-transaction))))

      (maxclaims::with-adb
        (let ((max (postmodern:query (:select (:max 'transaction_id)
                                              :from 'claim-transaction)
                                     :single))
              new)
          (warn "Starting at ~A" max)
        (postmodern:with-transaction (foo)
          (loop :for row in (rest csv-transaction)
             :do  (warn "Trying this row ~A~% :" row)
             (warn "Trying this real ~A~% :" (csv-transaction-amount row))
             (let* ((query (create-import-transaction-s-sql row))
                    (query (when query 
                             (s-sql:sql-compile query)))
                    (id (when (and query
                                   (not (string= (csv-transaction-amount row)
                                                 "38.7500"))
                                   (not (string= (csv-transaction-payee row)
                                                 "MASTERCLEAN")))                                   (warn "Trying this query ~A~% :" query)
                              (warn "Already have ~A" (length new))
                              (postmodern:query query :single))))

                   (when query
                     (unless (>= max (or id (+ max 1)))
                       (push query new))
                     (warn "~A ~A ~A ~A" query max id (ignore-errors (>= max id))))))

          (postmodern:abort-transaction foo))
        (nreverse new)))))

 (defun import-ipg-transactions-sql (&optional (trans (import-ipg-transactions)))
   (with-output-to-string (s)
     (format s "~{~A ;~%~}" trans)))


#+(or)  (defvar *attachment-csv*
    (with-open-file (stream "/home/maxclaims//xl-catlin/TCIM jul 18 2017/External/TCIM-Documents.csv") (cl-csv:read-csv stream)))

#+(or)  (defvar *attachment-paths*
    (mapcan #'cl-fad:list-directory
            (mapcan #'cl-fad:list-directory
                    (cl-fad:list-directory 
                     #P"/home/maxclaims/xl-catlin/External/53640/"))))

  (defun imported-claim-id (iclaim-id)
    (maxclaims::with-adb
        (postmodern:query
         (:select 'claim-id
                  :from 'import-claim
                  :where (:= 'import-claim-id iclaim-id))
         :single)))

  (defun import-claim-id (claim-id)
    (maxclaims::with-adb
        (postmodern:query
         (:select 'import-claim-id
                  :from 'import-claim
                  :where (:= 'claim-id claim-id))
         :single)))

  (defvar *non-imports* nil)



 (defun import-claim-coverage-analysis-sql
     (&key (file (cl-fad:merge-pathnames-as-file
                  (user-homedir-pathname)
                  "Downloads/TCIM-Claims.csv"))
        (csv-claim
         (with-open-file (f file)
           (cl-csv:read-csv f))))
   (let ()
     (create-struct 'csv-claim (first csv-claim))
     (with-output-to-string (s)
       (loop for claim in (rest csv-claim)
          :do
            (let ((date (csv-claim-date-opened claim))
                  (claim (parse-integer (csv-claim-claim claim)))
                  (notes (csv-claim-coverage-analysis claim)))
              (unless (equalp notes "")
                (format s"~A ;~%" (s-sql:sql-compile
                                   `(:select (:import-timecard-id (:select claim_id :from import-claim :WHERE (:= import-claim-id ,claim)), date, notes))))))))))

 (defun import-claim-notes.sql
     (&key 
        (file (cl-fad:merge-pathnames-as-file
               (user-homedir-pathname)
               "xl-catlin//External/TCIMFTCIMKTCIML -ClaimNotesHistory.csv"))
        (csv-claim-notes
         (with-open-file (f file)
           (cl-csv:read-csv f))))
   (let ()
     (create-struct 'csv-claim-notes (first csv-claim-notes))
     (with-output-to-string (s)
       (warn "~A" (length (rest csv-claim-notes)))
       (loop for claim in (rest csv-claim-notes)

          :do 
            (let ((date (csv-claim-notes-date-added claim))
                  (claim (parse-integer (csv-claim-notes-claim claim)))
                  (notes (csv-claim-notes-content claim)))
              (unless (equalp notes "")
                (format s"~A ;~%" (s-sql:sql-compile
                                   `(:select (:import-timecard-id (:select claim_id :from import-claim :WHERE (:= import-claim-id ,claim)), date, notes))))))))))




;; (defun find-unknown-claims (&key (file (cl-fad:merge-pathnames-as-file
;;                    (user-homedir-pathname)
;;                    "xl-catlin/jul21/TCIM Export4/External/TCIMK TCIML TCIMF-Claims.csv")))
;;     (let ((csv-claim
;;            (with-open-file (f file)
;;              (cl-csv:read-csv f))))
;;       (create-struct 'csv-claim (first csv-claim))
;;       (cons (first csv-claim)
;;             (loop :for claim :in (rest csv-claim)
;;          :for num := (parse-integer (csv-claim-claim claim))
;;          :for id := (imported-claim-id num)
;;          :unless id
;;          :collect claim))))

;; (defvar *unknown* (rest (find-unknown-claims)))

;; (defun find-unknown-transactions (&key (file (cl-fad:merge-pathnames-as-file
;;                                               (user-homedir-pathname)
;;                                               "xl-catlin/jul21/TCIM Export4/External/TCIMK TCIML TCIMF-ReserveItems.csv")))
;;    (let* ((nums (mapcar #'csv-claim-claim *unknown*))
;;           (trans
;;            (with-open-file (f file)
;;              (cl-csv:read-csv f)))
;;           (trans (progn
;;                    (create-struct 'csv-transaction (first trans))
;;                    (cons (first trans)
;;                          (remove-if-not (lambda (x)
;;                                           (member (csv-transaction-claim x) nums :test #'equalp))
;;                                         (rest trans))))))

;;      trans))
;;  (defun find-trisha-claims (&key (file (cl-fad:merge-pathnames-as-file
;;                    (user-homedir-pathname)
;;                    "xl-catlin/jul21/TCIM Export4/External/TCIMK TCIML TCIMF-Claims.csv")))
;;     (let ((csv-claim
;;            (with-open-file (f file)
;;              (cl-csv:read-csv f))))
;;       (create-struct 'csv-claim (first csv-claim))
;;       (cons (first csv-claim)
;;             (loop :for claim :in (rest csv-claim)
;;             :for name := (csv-claim-insured claim)
;;          :when (string= name "Litière Pouliot Inc")
;;                :collect claim))))

;;  (defun find-trisha-transactions (&key (file (cl-fad:merge-pathnames-as-file
;;                                               (user-homedir-pathname)
;;                                               "xl-catlin/jul21/TCIM Export4/External/TCIMK TCIML TCIMF-ReserveItems.csv")))
;;    (let* ((nums (mapcar #'csv-claim-claim (find-trisha-claims)))
;;           (trans
;;            (with-open-file (f file)
;;              (cl-csv:read-csv f)))
;;           (trans (progn
;;                    (create-struct 'csv-transaction (first trans))
;;                    (cons (first trans)
;;                          (remove-if-not (lambda (x)
;;                                           (member (csv-transaction-claim x) nums :test #'equalp))
;;                                         (rest trans))))))

;;      trans))

;;  (defun find-trisha-notes (&key (file (cl-fad:merge-pathnames-as-file
;;                                               (user-homedir-pathname)
;;                                               "xl-catlin/jul21/TCIM Export4/External/TCIMK TCIML TCIMF-ClaimNotesHistory.csv")))
;;    (let* ((nums (mapcar #'csv-claim-claim (find-trisha-claims)))
;;           (trans
;;            (with-open-file (f file)
;;              (cl-csv:read-csv f)))
;;           (trans (progn
;;                    (create-struct 'csv-note (first trans))
;;                    (cons (first trans)
;;                          (remove-if-not (lambda (x)
;;                                           (member (csv-note-claim x) nums :test #'equalp))
;;                                         (rest trans))))))

;;      trans))

  (defun csv-claim-ecm-status (csv-claim)
    (let ((status (csv-claim-status csv-claim)))
      (case (intern status)
        (c "Closed")
        (o "Open")
        (t (error "Unknown status ~A ~%~A" status csv-claim)))))


  (defun ensure-status (&key (file (cl-fad:merge-pathnames-as-file
                                    (user-homedir-pathname)
                                    "xl-catlin/jul21/TCIM Export4/External/TCIMK TCIML TCIMF-Claims.csv")))
    (let ((csv-claim
           (with-open-file (f file)
             (cl-csv:read-csv f)))
          wrongs)
     ; (create-struct 'csv-claim (first csv-claim))
      (maxclaims::with-adb 
        (loop for claim in (rest csv-claim)        
           :do (let* ((number (parse-integer (csv-claim-claim claim)))
                      (status (or (ignore-errors (csv-claim-ecm-status claim))
                                  (csv-claim-status claim)))
                      (ecm-id (imported-claim-id number))
                      (claim-status (when ecm-id
                                      (postmodern:query (:select 'status :from 'claim :where (:= 'claim-id ecm-id)) :single))))
                 (unless (or (not ecm-id)
                             (and status (string= status claim-status)))
                   (push (list :claim ecm-id
                               :imported-from number
                               :theirs (or status
                                           (csv-claim-status claim))
                               :ours claim-status)
                         wrongs)
                   (warn
                    "#~A has differing status. Ours ~A theirs ~A#~A"
                    ecm-id claim-status (or status
                                            (csv-claim-status claim))
                    number))
                 (if ecm-id
                     (warn "Imported OK! ~A" ecm-id)
                     (warn "NOt Imported! ~A" number)))
           :finally (return wrongs)))))
     


          




  ;;(loop for path in *attachment-paths* :do (print (import-attachment path)))
