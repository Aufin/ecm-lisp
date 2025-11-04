;;; Release/image dump/initial run script for Maxclaims

;;; Todo:
;;; - Command line args instead of pushnew *features* ...
;;; - central-registry -> asdf source registry
;;; - more configurable...ish (assumes you want a binary in ~/bin/,
;;;                            hardcoded ~/.../ location for sources, etc)

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

;;; todo: use the asdf2 source registry
(mapcar (lambda (x)  (pushnew x asdf:*central-registry*))
	(cons #P"/home/maxclaims/MaxclaimsNew/maxclaims/"
	      (directory "/home/maxclaims/MaxclaimsNew/build-dep/*")))

(mapcar #'ql:quickload 
	'(contextl parse-number swank yaclml trivial-garbage postmodern cl-fad net-telent-date rfc2109 usocket local-time cl-ppcre puri rfc2388-binary trivial-gray-streams))

(ql:quickload :maxclaims :verbose t)

(ql:quickload :ucw.httpd)
(ql:quickload :swank)

(defun maxclaims-image-pathname ()
  (merge-pathnames
    (make-pathname :directory '(:relative "bin")
		   :name (format nil "maxclaims_~AY~A"
				 (asdf:component-version (asdf:find-system :maxclaims))
				 (local-time:format-timestring nil (local-time:universal-to-timestamp
								    (get-universal-time))
							       :format '(:year #\- (:month 2) #\- (:day 2)
									 #\T (:hour 2) #\: (:min 2))))
		   :type "bin")
    (user-homedir-pathname)))

(defun dump-maxclaims-image (&optional (output (maxclaims-image-pathname)))
  (save-lisp-and-die output
		     :executable t
		     :toplevel (lambda () 
				 ;; disable ldb
				 (sb-alien:alien-funcall
				  (sb-alien:extern-alien "disable_lossage_handler" (function sb-alien:void)))
					;(swank:create-server)
				 (maxclaims::startup-maxclaims)
				 (sb-impl::toplevel-init))))

(defun make-release-image ()
  "Set up ~/bin/maxclaims.bin symlink and dump the release image"
  (let ((release-path (maxclaims-image-pathname))
	(binary-path (merge-pathnames "bin/maxclaims.bin" (user-homedir-pathname))))
    (handler-case  (sb-posix:unlink binary-path)
      (sb-posix:syscall-error (e)
	(or  (= sb-posix:enonet (sb-posix:syscall-errno e)) (signal e))))
    (sb-posix:symlink release-path binary-path)
    (dump-maxclaims-image release-path)))


#+maxclaims-make-image (dump-maxclaims-image)
#+maxclaims-make-release (make-release-image)

(setf ucw-core::*debug-on-error* nil)
(maxclaims::startup-maxclaims)
