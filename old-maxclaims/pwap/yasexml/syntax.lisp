#+quicklisp '#.(ql:quickload '("closure-html" "cxml"))
(defpackage :pwap/yasexml/syntax
  (:documentation 
   "YASEXML: Yet Another Symbolic Expression eXtensible Markup Language")
  (:use :cl)
  (:import-from :stp)
  (:import-from :pwap/yasexml/unparse
		#:make-unparse-handler
		#:call-with-document
		#:call-with-element
		#:characters
		#:unescaped)
  (:export #:<>
	   #:wrap-in-tag
	   #:*handler* 
	   #:*document*
	   #:*element*))
(in-package :pwap/yasexml/syntax)

(defvar *handler*)
(defvar *document*)
(defvar *element*)

(defgeneric call-with-tag (function 
			   tag &rest tag-attributes)
  (:method (fn (tag (eql :handler)) &rest make-unparse-handler-args)
    (let ((*handler* 
	   (apply #'make-unparse-handler make-unparse-handler-args)))
      (funcall fn)))
  (:method (fn (tag (eql :document)) &rest call-with-document-keys)
    (apply #'call-with-document 
	   *handler* 
	   (lambda (*document*) 
	     (funcall fn))
	   call-with-document-keys))
  
  (:method (fn tag &rest tag-attributes)
    (call-with-element *handler* (cons tag tag-attributes) 
		       (lambda (*element*)  
			 (funcall fn))))    
  (:method (fn (tag (eql :text)) &rest text)
    (map nil #'(lambda (text)
		 (characters *handler* (princ-to-string text))) text)
    (funcall fn))
  (:method (fn (tag (eql :unescaped)) &rest text)
    (map nil (lambda (text)
	       (unescaped *handler* text)) text)
    (funcall fn)))
  
(defgeneric wrap-in-tag (tag function)
  (:method ((tag symbol) f)
    (call-with-tag f tag))
  (:method ((tag string) f)
    (call-with-tag f :text tag))
  (:method ((tag list) f)
    (apply #'call-with-tag f tag)))

(defmacro <> (tag &body body)
  `(wrap-in-tag
    , (typecase tag 
	(symbol `',tag)
	(list
	 (if (let* ((*package* (find-package :cl))
		    (tag-prefix (aref (princ-to-string tag) 0)))
	       (or (eql #\` tag-prefix)
		   (eql #\' tag-prefix)))
	     ;; This must be a quote or backquote, so pass it along.
	     tag
	     `(list ',(first tag)
		    ,@(rest tag))))
	(t tag))
      (lambda () ,@body)))

(setf (documentation '<> 'function)
 #.(symbol-name '#:|
Example : (<> (:sink (cxml:make-string-sink
		     :indentation 1))
	   (<> (test-tag :test-attribute "test-attribute-value")
	     (<> "Test Text as Tag" (<> (br)))
	     (<> (:text "Test :TEXT as Tag") 
	       (<> ("foo bar=baz" :bat "?"))
	       (<> (:text "Test " ":TEXT" "many strings"))
	       (<> `(:text ,(concatenate 
			     'string "Test " ":TEXT "
			      "many strings with backquote " 
			      "and concatenate" ))
		 (<> `(,(funcall (constantly 'backquoted-tag-name))
			,@(list 'list "attribute")))))))
	     
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<test-tag test-attribute=\"test-attribute-value\">
 Test Text as Tag
 <br/>
 Test :TEXT as Tag
 <foo bar=baz bat=\"?\"/>
 Test
 :TEXT
 many strings
 Test :TEXT many strings with backquote and concatenate
 <backquoted-tag-name list=\"attribute\"/>
</test-tag>"|))

;; Copyright (c) 2013 Drew Crampsie <drewc@smug.im>
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
