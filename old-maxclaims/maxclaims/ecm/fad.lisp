(uiop:define-package :ecm/fad
    (:use :cl-fad :cl)
  (:import-from :uiop/run-program)
  (:reexport :cl-fad)
  (:export :escape-filename))
(in-package :ecm/fad)

(defun escape-filename (filename &key (slash-escape #\|))
  (uiop:run-program
    (concatenate 'string "bash <<'EOF'
printf '%q' " (uiop/run-program:escape-sh-token
		      (substitute slash-escape #\/  filename))"
EOF")   
    :output '(:STRING :STRIPPED T)))


  
