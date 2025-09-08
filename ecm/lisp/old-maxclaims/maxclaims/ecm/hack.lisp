(defpackage :ecm/hack
  (:use :cl)
  (:import-from :cxml)
  (:import-from :cxml-stp)
  (:import-from :lparallel)
  (:import-from :quux-hunchentoot))
(in-package :ecm/hack)

(defun cxml::split-qname (qname)
  ;; (declare (type runes:simple-rod qname))
  (let ((pos (position  #/: qname)))
    (if pos
	      (let ((prefix (subseq qname 0 pos))
	            (local-name (subseq qname (1+ pos))))
	        (when (zerop pos)
	          (cxml::wf-error nil "empty namespace prefix"))
	        (if (cxml::nc-name-p local-name)
	            (values prefix local-name)
	            (cxml::wf-error nil "~S is not a valid NcName."
			                  (cxml::rod-string local-name))))
	      (values () qname))))

(defun cxml-stp-impl::make-element (name &optional (uri ""))
  "@arg[name]{string, a QName or NCName}
   @arg[uri]{a string, the namespace URI}
   @return{an @class{element}}
   @short{This function creates an element node of the given name.}"
  ;; (check-type name runes:rod)
  (let ((result (make-instance 'cxml-stp-impl::element)))
    (multiple-value-bind (prefix local-name)
	      (cxml::split-qname name)
      (setf prefix (or prefix ""))
      (setf (cxml-stp-impl::namespace-prefix result) prefix)
      (setf (cxml-stp-impl::namespace-uri result) uri)
      (setf (cxml-stp-impl::local-name result) local-name))
    result))

(setf (symbol-function 'quux-hunchentoot::WRAPPED-ERROR-P)
      (symbol-function 'lparallel.kernel::wrapped-error-p))
