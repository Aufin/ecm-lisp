(defpackage :ecm/make
  (:use cl)
  (:import-from :ecm/start)
  (:import-from :uiop/run-program))
(in-package :ecm/make)

(declaim (optimize (speed 3) (safety 0)))

;; (defun find-current-git-rev ()
;;   (let* ((dir (asdf:system-source-directory :ecm))
;; 	 (cmd (concatenate 'string "cd " (namestring dir)
;; 			   "; git rev-parse HEAD")))
;;     (uiop/run-program:run-program cmd
;;        :output '(:STRING :STRIPPED T)
;;        :error-output :output)))

;; (defparameter *current-git-rev*
;;   (find-current-git-rev))

(defun toplevel ()
  (ecm/start:start)
  (progn
    (in-package :maxclaims)
    (sb-impl::toplevel-init)))

;;(sb-posix:chdir (asdf:system-source-directory :ecm))

(defun make ()
  (sb-ext:save-lisp-and-die
   "ecm-application"
   :executable t
   :toplevel #'toplevel
   ;;   :compression 9
   :save-runtime-options t
   :purify t))

