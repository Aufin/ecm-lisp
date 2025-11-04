#+quicklisp '#.(ql:quickload '("cxml" "closure-html"))

(defpackage :pwap/yasexml/template
  (:documentation 
   "Template YASEXML: Using XML/HTML templates and CL, together!")
  (:use :cl)
  (:export))

(in-package :pwap/yasexml/template)



