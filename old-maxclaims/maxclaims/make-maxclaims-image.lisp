(cl:declaim (optimize (space 3) (debug 0)))
(cl:push #P"/home/drewc/src/maxclaims/" asdf:*central-registry*)
(cl:push #P"/home/drewc/src/clbuild/systems/" asdf:*central-registry*)
(cl:push #P"/home/drewc/src/clbuild/source/iolib/src/" asdf:*central-registry*)
(cl:require :maxclaims)
(cl:require :ucw.httpd)



(save-lisp-and-die "maxclaims.bin" :executable t
		   :toplevel 
		   (lambda () 
		     ;;; disable ldb
		     (sb-alien:alien-funcall
		      (sb-alien:extern-alien "disable_lossage_handler" (function sb-alien:void)))
		     (swank:create-server)
		     (maxclaims::startup-maxclaims)
		     (sb-impl::toplevel-init)
		   ))
