(defpackage :maxclaims/data-entity/app-user
  (:use :cl)
  (:import-from :maxclaims
		#:app-user
		#:username
		#:user-read-only-p
		#:app-user.admin 
		#:app-user.password
		#:app-user.person
		#:app-user.username
		#:app-user.contracts
		#:app-user.app-user-id
		#:app-user-id-rolename
		#:with-adb
		#:with-udb
		#:$app-user
		#:select
		#:user-can-edit-p)
  (:import-from :postmodern 
		#:execute
		#:query)
  (:import-from :rofl
		#:insert-object
		#:update-object)
  (:export #:app-user-person-or-username
	   #:app-user-read-only-p
	   #:app-user-administrator-p
	   #:app-user-contracts
	   #:app-user-rolename
	   #:insert-app-user
	   #:update-app-user))

(in-package :maxclaims/data-entity/app-user)

(defun app-user-person-or-username (app-user)
  (or (ignore-errors (app-user.person app-user))
      (app-user.username app-user)))

(defun app-user-read-only-p (app-user)
  (with-adb  
    (user-read-only-p app-user)))

(defun app-user-administrator-p (app-user)
  (with-adb  
    (app-user.admin app-user)))

(defun app-user-contracts (app-user)
  (with-adb 
    (mapcar #'maxclaims::app-user-contract.contract 
	    (app-user.contracts app-user))))


(defun app-user-rolename (user)
  (app-user-id-rolename (app-user.app-user-id user)))

(defun select-app-user-role (user)
  (car (select '* 
	       :from 'pg_roles 
	       :where `(:= rolname 
			   ,(app-user-rolename user)))))

(defun %create-app-user-privileges (user)
  (labels ((f (string &rest args)
	     (apply #'format nil string args))
	   (revoke (group role)
	     (execute (f "REVOKE ~A FROM ~A" group role)))
	   (grant (group role)
	     (execute (f "GRANT ~A TO ~A" group role))))
    (let ((user-role (select-app-user-role user))
	  (rname (app-user-rolename user))
	  (pw (app-user.password user))
	  (contracts (app-user.contracts user))
	  (admin (ignore-errors (app-user.admin user))))
   ;; (break "~A ~A con:~A" rname pw admin)

    (unless (or user-role (string-equal "" pw))
      ;;(break "~A ~A" user pw)
      (query (format nil "CREATE ROLE ~A WITH LOGIN PASSWORD '~A'"
		     rname
		     pw
		     ))
      (setf user-role (select-app-user-role user)))
      
    (if (string-equal "" pw)
	(execute (format nil "DROP ROLE IF EXISTS ~A" rname))
	(progn 
	  (revoke "mr_read" rname)
	  (revoke "mr_update" rname)
	  (revoke "mr_insert" rname)
	  (revoke "mr_admin" rname)
      
	  (execute 
	   (format nil "ALTER ROLE ~A WITH PASSWORD '~A'" rname pw))

	  (grant "mr_read" rname)
	  (grant "mr_report" rname)
	  (unless contracts 
	    (grant "mr_update" rname)
	    (grant "mr_insert" rname))
	  (when admin
	    (grant "mr_admin" rname)))))))

(defun create-app-user-privileges (user)
  (with-udb (%create-app-user-privileges user)))

(defun create-all-app-user-privileges ()
  (with-adb 
    (let ((u (maxclaims::select-objects 'app-user)))
      (dolist (a u)
	(%create-app-user-privileges a)))))

(flet ((%uiu (fn app-user)
	 (let ((u (funcall fn app-user)))
	   (prog1 u (create-app-user-privileges u)))))
  (defun insert-app-user (app-user)
    (%uiu #'insert-object app-user))
  (defun update-app-user (app-user)
    (%uiu #'update-object app-user)))

