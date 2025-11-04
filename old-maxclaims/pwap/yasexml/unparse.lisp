#+quicklisp '#.(ql:quickload '("closure-html" "cxml"))

(defpackage :pwap/yasexml/unparse
  (:documentation 
   "YASEXML: Unparse")
  (:use :cl)
  (:import-from :runes #:rod)
  (:export #:*handler* 
	   #:call-with-handler
	   #:call-with-document	   
	   #:call-with-element))
(in-package :pwap/yasexml/unparse)

(defvar *handler*)

(defvar *document*)

(defun call-with-document (function 
			   &key 
			     document
			     name
			     public-id system-id)
   
    (funcall function 
	     (sax:start-document  
	      handler 
	      :name name
	      :public-id public-id
	      :system-id system-id))
    (end-document handler))

(defclass unparse-handler ()
  ((handler :initarg :handler
	    :accessor unparse-handler-handler)))

(defclass hax-unparse-handler (unparse-handler)
  ())

(defun make-unparse-handler (handler &key (class 'unparse-handler))
  (make-instance class 
		 :handler handler))

(symbol-macrolet (($unparse-handler (unparse-handler-handler 
				     unparse-handler)))
  
  (defgeneric start-document (handler 
			      &key name
				public-id 
				system-id)
    (:method ((unparse-handler unparse-handler)
	      &key name 
		public-id 
		system-id)
      (let ((h $unparse-handler))
	(prog1 (sax:start-document h)
	  (when system-id
	    (sax:start-dtd h name public-id 
			   system-id)
	    (sax:end-dtd h)))))
    (:method ((unparse-handler hax-unparse-handler)
	      &key name 
		public-id 
		system-id)
      (let ((h $unparse-handler))
	(hax:start-document h name public-id 
			    system-id))))

  (defgeneric end-document (handler)
    (:method ((unparse-handler unparse-handler))
      (sax:end-document $unparse-handler))
    (:method ((unparse-handler hax-unparse-handler))
      (hax:end-document $unparse-handler)))

  (defun call-with-document (handler function 
			     &key (name "yasexml") 
			       public-id system-id)
    #. (symbol-name 
	'#:|=> result
Arguments and Values: 
 handler --- an UNPARSE-HANDLER
 function --- a function to call with the result of START-DOCUMENT
 public-id --- 
 system-id
Examples:

> (call-with-document 
   (make-unparse-handler (cxml-stp:make-builder))
   (lambda (document) (print document)) 
    :name "foo"
    :public-id "bar"
    :system-id "baz")
 		       
 (#.(CXML-STP-IMPL::DOCUMENT
     :CHILDREN '(#.(CXML-STP-IMPL::DOCUMENT-TYPE
                    #\| :PARENT of type DOCUMENT \|#
                    :ROOT-ELEMENT-NAME "foo"
                    :SYSTEM-ID "baz"
                    :PUBLIC-ID "bar"   
                    :INTERNAL-SUBSET "")))) 
=> #.(CXML-STP-IMPL::DOCUMENT
      :CHILDREN '(#.(CXML-STP-IMPL::DOCUMENT-TYPE
		     #\| :PARENT of type DOCUMENT \|#
		     :ROOT-ELEMENT-NAME "foo"
		     :SYSTEM-ID "baz"
		     :PUBLIC-ID "bar"
		     :INTERNAL-SUBSET ""))) |)
    (funcall function 
	     (start-document  
	      handler 
	      :name name
	      :public-id public-id
	      :system-id system-id))
    (end-document handler))

  (defgeneric rod-name (name &key print-case)
    (:method ((name symbol) &key (print-case :downcase))	  
      (let* ((*print-case* 
	      (if (find-if #'lower-case-p 
			   (symbol-name name))
		  *print-case* 
		  print-case)))
	(rod-name (princ-to-string name))))
    (:method ((name string)  &key &allow-other-keys)
      (runes:rod name)))

  (defgeneric start-element (handler 
			     element
			     &rest attributes)
    (:method ((unparse-handler unparse-handler) 
	      element
	      &rest attributes)
      (sax:start-element 
       $unparse-handler 
       NIL ;; TODO namespace-uri
       NIL;; TODO local-name 
       (rod-name element)
       ))

    (:method ((unparse-handler hax-unparse-handler) 
	      element
	      &rest attributes)
      	(hax:start-element
	 $unparse-handler 
	 (rod-name element)
	  (loop :for (name value) :on attributes :by #'cddr
	    :collect (hax:make-attribute
		      (rod-name name)
		      (princ-to-string value))))))

  (defgeneric end-element (unparse-handler 
			   name &key local-name
				  namespace-uri)
    (:method ((unparse-handler unparse-handler) 
	      name  &key (local-name name)
		      (namespace-uri nil))
      (sax:end-element $unparse-handler 
		       namespace-uri 
		       (rod-name local-name) 
		       (rod-name name)))
        (:method ((unparse-handler hax-unparse-handler) 
	      name  &key &allow-other-keys)
      (hax:end-element $unparse-handler  
		       (rod-name name))))

  (defun call-with-element (handler element-description function )
    "=> result

Arguments and Values:
 handler --- an [unparse-handler]
 element-description --- an [atom] or a [list] 
 function --- any [function] arguments --- a [list] of values
 result --- the [values] that /function/ returns

Description: 

 Wrapped in an [element], calls the /function/ with /arguments/.

 /Element-description/ can be an [atom] or a [list]. 

 For an [atom], it is the _name_ of the element, and should be a
 [symbol] or a [string].

 Otherwise it is a [list]. The first item should be the _name_, and
 the rest should be a [plist] of the attribute names and values.
"
    (destructuring-bind (name &rest attributes)
	(if (listp element-description) 
	    element-description 
	    (list element-description))
      ))

  (defgeneric characters (handler characters)
    (:method ((unparse-handler unparse-handler) characters)
      (sax:characters $unparse-handler 
		      (rod characters)))
    (:method ((unparse-handler hax-unparse-handler) characters)
      (hax:characters $unparse-handler 
		      (rod characters))))

  (defgeneric unescaped (handler characters)
    (:method ((unparse-handler unparse-handler) characters)
      (sax:unescaped $unparse-handler 
		     (rod characters)))
        (:method ((unparse-handler unparse-handler) characters)
      (sax:unescaped $unparse-handler 
		     (rod characters)))))

;; Copyright (c) 2013 Drew Crampsie <drewc@drewc.org>
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; ")AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
