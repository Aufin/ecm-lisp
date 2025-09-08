(in-package :maxclaims)

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
   (with-adb 
    (execute 
     (format nil "ALTER USER ~A CREATEUSER" 
	     (app-user-rolename $app-user))))
   (with-udb (%create-app-user-privileges user)))

(defun create-all-app-user-privileges ()
  (with-adb 
    (let ((u (select-objects 'app-user)))
      (dolist (a u)
	(%create-app-user-privileges a)))))



#+(or) (unless (select-app-user-role user)
		       (<:as-html "NO ROLE "))

