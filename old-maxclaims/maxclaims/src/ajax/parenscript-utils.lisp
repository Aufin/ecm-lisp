;;; Utilities for use with parenscript

;;; Portions taken from ucw_ajax/src/parenscript-utils.lisp.
;;; - dojo dependencies have been ported to YUI2.
;;; - iterate has been replaced with loop

(in-package #:maxclaims)

(defmacro with-unique-dom-ids (names &body body)
  `(let ,(loop for name in names
	    collect `(,name (js:gen-js-name-string)))
     ,@body))



