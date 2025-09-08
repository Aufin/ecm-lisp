#!/bin/bash

BASE_DIR="$(readlink -f $(dirname "$0"))"

cd $BASE_DIR

echo $BASE_DIR

## exit 0

sbcl --eval "(require :asdf)"\
     --eval "(asdf:initialize-source-registry 
              '(:source-registry :inherit-configuration 
                (:tree \"/$BASE_DIR/../\") ))"\
     --eval "(let ((quicklisp-init 
                    (merge-pathnames \"quicklisp/setup.lisp\"
                                   (user-homedir-pathname))))
              (when (probe-file quicklisp-init)
               (load quicklisp-init)))"\
     --eval "(ql:quickload '(
                            :split-sequence
                           
                         ))"\
     --eval "(with-open-file (*ERROR-OUTPUT* \"/dev/null\"
                              :direction :output :if-exists :overwrite)
             (asdf:load-system :maxclaims))" --eval "(maxclaims::load-configuration-file)" --eval "(with-open-file (foo \"$(basename "$0")\") (loop until (string= (read-line foo) \"exit\")) (load foo))" "$@"

exit

(require :postmodern)

(defun load-compiled (filename)
  (let ((fasl-filename (compile-file-pathname filename)))
    (when (or (not (probe-file fasl-filename))
	      (>= (file-write-date filename)
		  (file-write-date fasl-filename)))
      (compile-file filename))
    (load fasl-filename)))

(load-compiled "../database/quick-migrate.lisp")

(defparameter *database-parameters* maxclaims::*db-admin-parameters*)

(setf quick-migrate:*migration-file* "../database/migrations.lisp")

(destructuring-bind
      (&optional version &rest junk)
    (cdr sb-ext:*posix-argv*)
  (when (or junk (not version))
    (format t "usage: migrate-database.sh <version>~%~%Where <version> is an integer schema version or :latest~%")
    (sb-ext:quit :unix-status 1))
  (handler-bind ((error (lambda (condition)
		          (format *error-output* "Unhandled error: ~S~%  ~A~%~%" condition condition)
			  (sb-debug:backtrace most-positive-fixnum *error-output*)
                          (format *error-output* "~%~%Unhandled error: ~S~%  ~A~%~%" condition condition)
			  (sb-ext:quit :unix-status 1))))
    (quick-migrate:load-migrations)
    (quick-migrate:migrate-to *database-parameters* (read-from-string version))))

(sb-ext:quit)
