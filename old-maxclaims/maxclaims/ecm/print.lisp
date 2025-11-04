(defpackage :ecm/print
  (:use :cl)
  (:export #:write-money
           #:princ-money-to-string))
(in-package :ecm/print)

(defun write-money (money &key (stream *standard-output*)
                            (prefix "$"))
  (format stream 
          "~A~$"
          prefix
          (typecase money 
            (rational (coerce money 'double-float))
            (t money))))

(defun princ-money-to-string (money 
                              &key 
                                (prefix "$"))
  (with-output-to-string (s) (write-money money :stream s :prefix prefix)))
  

