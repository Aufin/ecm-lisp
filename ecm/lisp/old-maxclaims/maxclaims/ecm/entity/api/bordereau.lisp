(defpackage :ecm/entity/api/bordereau
  (:use :cl)
  (:export #:make-bordereau-instance)
;  (:import-from :ecm/user #:with-user)
 ; (:import-from :ecm/ml #:<>)
 ; (:import-from :ecm/ps #:{} #:|.|)
 ; (:import-from :ecm/ui/spreadsheet
;		#:<spreadsheet-type-select>
;		#:<download-spreadsheet>)
                                        ; (:import-from :ecm/report/agency-bordereau)
  )

;; https://superuser.com/questions/1135850/how-do-i-run-a-libreoffice-macro-from-the-command-line-without-the-gui

(in-package :ecm/entity/api/bordereau)

(defstruct bordereau
  start-date
  end-date
  ;; %fields is a function that, when called with the bordereau, outputs the columns s-sql for the query
  (%fields
   (lambda (_)
     (declare (ignore _))
     (maxclaims/report/bordereau:default-bordereau-fields)))
  ;; %where is a function that, when passed the bordereau, outputs the "where" s-sql for the query
  (%where (constantly '(:= claim.status "Open")))
  (risk-type nil)
  (prefix-currency t)
  (run-start-time nil)
  (run-end-time nil)
  (status nil)
  thread
  %id
  meta
  (json nil))

(defvar *bordereaux-table* (make-hash-table :test #'equalp))

(defun make-bordereau-instance (type &rest initargs)
  (let ((bdx (apply #'make-bordereau-thread type initargs)))
    (setf (gethash (bordereau-id bdx) *bordereaux-table*) bdx)
    bdx))

(defun get-bordereau-instance (id)
  (gethash id *bordereaux-table* nil))

(defun make-bordereau-thread (type &rest initargs)
  (let* ((sym (intern (format nil "MAKE-~A" type) (symbol-package type)))
         (con (and (fboundp sym) (symbol-function sym))))
    (when (not con)
      (error "Cannot find constructor named ~A" sym))
    (let* ((bdx (apply con initargs))
           (id (bordereau-id bdx))
           (user ecm/user::*user*)
           (thr (bt:make-thread
                 (lambda ()
                   (ecm/user:bind-user
                    user (lambda ()
                           (maxclaims::with-udb
                             (run-bordereau bdx)))))
                 :name id)))
      (prog1 bdx
        (setf (bordereau-thread bdx) thr)))))

(defgeneric bordereau-fields (bordereau)
  (:method (bdx) (funcall (bordereau-%fields bdx) bdx)))
(defgeneric bordereau-where (bordereau)
  (:method (bdx) (funcall (bordereau-%where bdx) bdx)))

(defgeneric bordereau-sql (bordereau)
  (:method (bdx)
    (let ((risk-type (bordereau-risk-type bdx)))
      (second
       (maxclaims/report/bordereau:find-bordereau
        nil
        :fields (bordereau-fields bdx)
        :where `(:and ,(bordereau-where bdx)
                      ,@(when risk-type
		          `((:= risk.risk-type-name ,risk-type))))
	:prefix-currency (bordereau-prefix-currency bdx)
        :run nil
        :start-date (bordereau-start-date bdx)
        :end-date (bordereau-end-date bdx))))))

(defun get-unix-time ()
  (local-time:timestamp-to-unix
   (local-time:universal-to-timestamp (get-universal-time))))

(defgeneric bordereau-started-on (bdx)
  (:method (bdx)
    (or
     (bordereau-run-start-time bdx)
     (let ((st (get-unix-time)))
       (setf (bordereau-run-start-time bdx) st)
       st))))

(defgeneric bordereau-filename (bdx)
  (:method (bdx) (format
                  nil "~A-~A.csv"
                  (class-name (class-of bdx))
                  (bordereau-started-on bdx))))

(defgeneric bordereau-file-directory (bdx)
    (:method (bdx)
      #P"/tmp/ecm-bordereau-csv/"))

(defgeneric bordereau-pathname (bdx)
  (:method (bdx)
    (merge-pathnames
     (bordereau-filename bdx)
     (bordereau-file-directory bdx))))


(defvar *bordereau-prep-number* 0)
(defun bordereau-prep-id ()
  (format nil "BDX-ID-~A" (incf *bordereau-prep-number*)))
(defgeneric bordereau-id (bordereau)
  (:method (bdx) (or (bordereau-%id bdx)
                     (setf (bordereau-%id bdx)
                           (bordereau-prep-id)))))

(defgeneric bordereau-sql-to-prepare (bordereau)
  (:method (bdx)
    (format nil "CREATE TEMPORARY TABLE ~W AS (~A)"
            (bordereau-id bdx)
            (bordereau-sql bdx))))


(defgeneric bordereau-function (bdx)
  (:documentation "By default this takes a BORDEREAU and produces a .csv file in BORDEREAU-PATHNAME")
  (:method (bdx)
    (lambda (&key
               (start-date (bordereau-start-date bdx))
               (end-date (bordereau-end-date bdx))
               &allow-other-keys)
      (let* ((sql (bordereau-sql-to-prepare bdx))
             (stmt (postmodern:prepare sql))
             (proper-end-date
               (maxclaims::with-adb
                 (postmodern:query
		              (s-sql:sql-compile
                   `(:SELECT (:- (:type ,end-date date) (:raw  "INTERVAL '1 second'"))))
		              :single))))
        ;;(warn "sql: ~A" sql)
      (progn
        (funcall stmt start-date proper-end-date)
        (postmodern:execute
         (format
          nil
          "COPY (SELECT * FROM ~W)
            TO '~A' DELIMITER ',' CSV HEADER ENCODING 'WIN1252'"
          (bordereau-id bdx)
          (bordereau-pathname bdx)))
        (postmodern:execute (format nil "DROP TABLE ~W;" (bordereau-id bdx))))))))

(defun ensure-bordereau-output-directory (bdx)
  (let ((dir (cl-fad:pathname-directory-pathname
              (bordereau-pathname bdx))))
    (when (not (cl-fad:directory-exists-p dir))
      (ensure-directories-exist (cl-fad:pathname-directory-pathname
                                 (bordereau-pathname bdx))
                                :mode 511)
      (sb-posix:chmod dir #o777))))

(defgeneric run-bordereau (bordereau)
  (:method (bdx)
    (ensure-bordereau-output-directory bdx)
    (when (not (bordereau-status bdx))
      (setf (bordereau-status bdx) :started)
      (bordereau-started-on bdx)
      (handler-case
          (multiple-value-prog1
              (funcall (bordereau-function bdx))
          (setf (bordereau-run-end-time bdx) (get-unix-time))
          (setf (bordereau-status bdx) :finished))
        (error (c)
          (setf (bordereau-status bdx) :error)
          (setf (bordereau-meta bdx) c))))))


(defstruct (contract-bordereau
            (:include bordereau
             (:%where (lambda (b)
                        `(:= contract.contract-id
                             ,(contract-bordereau-contract-id b))))))
  contract-id)

(defmethod bordereau-sql ((bdx contract-bordereau))
 (call-next-method))

(defvar *test-bdx*)
(defun test-bordereau (&key (uid 2) (contract-id 4072))
  (let* ((user (maxclaims::with-adb (ecm/user:find-user-by-id uid)))
         (report (ecm/user:bind-user
                  user
                  (lambda ()
                    (make-bordereau-instance
                     'contract-bordereau
                     :contract-id contract-id
                     :start-date "2023-01-01"
                     :end-date "2023-02-01")))))

    (setf *test-bdx* report)
    (bordereau-id report)))

