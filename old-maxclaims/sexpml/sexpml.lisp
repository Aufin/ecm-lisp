(defpackage :sexpml
  (:use :cl)
  (:documentation "SEXPML - Symbolic Exression eXtensible Programmable Markup Language")
  (:export #:<>
	   #:<>-form
	   #:*symbol-home-package*
	   #:*sexpml-output*
	   #:define-tag
	   #:tag-attributes
	   #:tag-contents
	   #:*sexpml-indent*
	   #:sexpml-form
	   #:sexpml-attributes-bind
	   #:sexpml-attributes-plist
	   #:*sexpml-attributes*))
(in-package :sexpml)

(defun symbol-tag-p (symbol)
  (get symbol '<>))

(defun (setf symbol-tag-p) (value symbol)
  (setf (get symbol '<>) value))
  
(defmacro define-tag (name &body contents)
  `(etypecase ',name
     (symbol
      (setf (get ',name '<>) t)
      (defmethod sexpml-form ((tag (eql ',name))
			      &key attributes contents)
	(flet ((tag-attributes () attributes)
	       (tag-contents () contents))
	  ,@contents)))))

(defgeneric sexpml-form (name 
                         &key 
                           attributes
                           contents
                           &allow-other-keys)
  (:documentation "form n. 1. any object meant to be evaluated.
This function produces a form which, when evaluated, will generate some corresponding markup."))


      
  


(defgeneric sexpml-indent (name &key &allow-other-keys)
  (:method (name &key &allow-other-keys) t))

(defparameter *sexpml-output* *standard-output*
  "Where tags are output.")

(defvar *sexpml-indent* t)
(defvar *sexpml-indent-level*)
(setf (documentation '*sexpml-indent-level* 'variable)
      "The indentation level of this form")

(defvar *sexpml-mode* :html
    "Either :HTML or :XML")
      
(defvar *sexpml-forms*)

(defvar *sexpml-attributes* (list))

(defun sexpml-constant-p (thing)
  (or (stringp thing)
      (characterp thing)
      (numberp thing)
      (keywordp thing)))



(defun sexpml-attributes-plist (attributes)
  (let (plist)
    (loop :while attributes
       :do (let* ((key (pop attributes))
		  (key? (keywordp key))
		  (val (when key? (pop attributes))))
	     (when key?
	       (push key plist)
	       (push val plist)))
       :finally (return plist))))
  
(defmacro sexpml-attributes-bind ((&rest bindings) attributes &body body)
  (let* ((plist (gensym))
	 (atts '*sexpml-attributes*))
    `(let ((,atts ,attributes)
	   ,plist)
;       (break "~A" ,atts)
       (loop :while ,atts
	  :do (let* ((key (pop ,atts))
		     (key? (keywordp key))
		     (val (when key? (pop ,atts))))
		(when key?
		  (push key ,plist)
		  (push val ,plist))))
       (destructuring-bind (&key ,@bindings) (nreverse ,plist)
	 ,@body))))

(defun emit-princ (&rest items)
  "Emit to the current yaclml-code a form which will, at runtime,
   princ ITEM. 

   If (sexpml-constant-p ITEM) is true the princ will
   be done at compile time.

   If it is a cons at run time, is is a list of attributes."
  (dolist (item items *sexpml-forms*)
    (push (cond
            ((stringp item)
             item)
            ((keywordp item)
             (string-downcase (princ-to-string item)))
            ((sexpml-constant-p item)
             (princ-to-string item))
            (t
	     (let ((ritem (gensym)))
	       `(let ((,ritem ,item))
		  (typecase ,ritem 
		    (cons (princ-attributes ,ritem
					    :start-with-space t))
		    (null)
		    (t (princ ,ritem *sexpml-output*)))))))
          *sexpml-forms*)))

(defun emit-indentation ()
  (when *sexpml-indent*
    (emit-princ #\Newline)
    (emit-princ (make-string *sexpml-indent-level*
                             :initial-element #\Space))))

(defun emit-code (form)
  (push form *sexpml-forms*))

(defvar *escape-char-p*
  (lambda (char)
    (or (find char "<>&'\"")
        (> (char-code char) 127)))
  "Used by ESCAPE-STRING to test whether a character should be escaped.")

;;(declaim (inline escape-char))

(defun escape-char (char &key (test *escape-char-p*))
  (declare (optimize speed))
  "Returns an escaped version of the character CHAR if CHAR satisfies
the predicate TEST.  Always returns a string."
  (if (funcall test char)
    (case char
      (#\< "&lt;")
      (#\> "&gt;")
      (#\& "&amp;")
      (#\' "&#039;")
      (#\" "&quot;")
      (t (format nil (if (eq *sexpml-mode* :xml) "&#x~x;" "&#~d;")
                 (char-code char))))
    (make-string 1 :initial-element char)))

(defun escape-string (string &key (test *escape-char-p*))
  (declare (optimize speed))
  "Escape all characters in STRING which pass TEST. This function is
not guaranteed to return a fresh string.  Note that you can pass NIL
for STRING which'll just be returned."
  (let ((first-pos (position-if test string))
        (format-string (if (eq *sexpml-mode* :xml) "&#x~x;" "&#~d;")))
    (if (not first-pos)
      ;; nothing to do, just return STRING
      string
      (with-output-to-string (s)
        (loop with len = (length string)
              for old-pos = 0 then (1+ pos)
              for pos = first-pos
                  then (position-if test string :start old-pos)
              ;; now the characters from OLD-POS to (excluding) POS
              ;; don't have to be escaped while the next character has to
              for char = (and pos (char string pos))
              while pos
              do (write-sequence string s :start old-pos :end pos)
                 (case char
                   ((#\<)
                     (write-sequence "&lt;" s))
                   ((#\>)
                     (write-sequence "&gt;" s))
                   ((#\&)
                     (write-sequence "&amp;" s))
                   ((#\')
                     (write-sequence "&#039;" s))
                   ((#\")
                     (write-sequence "&quot;" s))
                   (otherwise
                     (format s format-string (char-code char))))
              while (< (1+ pos) len)
              finally (unless pos
                        (write-sequence string s :start old-pos)))))))

(defun emit-form (form)
  "Emits the code for FORM."
  (if (consp form)
      (let ((op (car form)))
        (cond
          #+(or)((eql 'sexpml:<> op)
           (emit-code 
            (let ((*sexpml-forms* nil))
              (<>-form (second form)
                       (rest (rest form))))))
          ((eql 'cl:progn op)
           (dolist (b (cdr form))
             (emit-form b)))
          (t (emit-code form))))
      (if (sexpml-constant-p form)
          (emit-princ (escape-string (princ-to-string form)))
          (emit-code form))))

(defun attributes-printer (attributes 
			   &key (start-with-space t)
			     (emit-princ #'emit-princ)
			     (emit-attribute-value #'emit-form))
  
  (when attributes
    (block attribute
      (let* ((attribute (pop attributes))
             (value
	      (cond
		;; First, make sure it is not nil. if so, return and
		;; try again
		((not attribute)
		 (return-from attribute))
		;; If it is a keyword, the value is next in line
		((keywordp attribute)
		 (pop attributes)))))
	(when
	    (and start-with-space
		 (not (listp attribute)))
	  (funcall emit-princ #\Space))
        (funcall emit-princ attribute)
        (when value
          (funcall emit-princ #\=)
          (if (eq t value)
              (funcall emit-princ attribute)
              (progn (funcall emit-princ #\")
                     (funcall emit-attribute-value value)
                     (funcall emit-princ #\"))))))
        (attributes-printer
         attributes 
         :emit-princ emit-princ
         :emit-attribute-value emit-attribute-value)))

(defun emit-attributes (attributes)
  (when (and attributes (not (every #'null attributes)))
    (if (and (listp (first attributes))
             (= (length attributes) 1)
             (eq (caar attributes)
                 'sexpml-attributes-to-string))
        (emit-princ (first attributes))
        (progn 
          (attributes-printer attributes :emit-attribute-value #'emit-princ)))))

(defun princ-attributes (attributes &key (escape-string #'escape-string)
				      (start-with-space t))
    (attributes-printer 
     attributes
     :emit-princ (lambda (v) 
                   (princ 
                    (funcall 
                     (if (characterp v) 
                         #'identity
                         escape-string )
                     (if (keywordp v)
                         (string-downcase (princ-to-string v))
                         (princ-to-string v)))
               *sexpml-output*))
     :emit-attribute-value (lambda (value)
                             (princ (funcall escape-string (princ-to-string value))
                                    *sexpml-output*))
     :start-with-space start-with-space))

(defun princ-attributes-to-string (attributes &key (escape-string #'escape-string))
  (with-output-to-string (*sexpml-output*)
    (princ-attributes attributes )))

(defun emit-open-tag (name &key attributes empty? (sexpml-mode *sexpml-mode*))
    "Emit the code required to print an open tag whose name is NAME and
with the attributes ATTRIBUTES."
    (incf *sexpml-indent-level* 2)
    (emit-princ "<")
    (emit-princ name)
    (emit-attributes attributes)
    (when (and (not empty?) *sexpml-indent*)
      (emit-indentation))
    (when (and empty? (eql sexpml-mode :xml))
      (emit-princ "/"))
    (emit-princ ">"))

(defun emit-close-tag (name &key empty?)
  "Emit the code required to print a close tag whose name is NAME."
  (when *sexpml-indent*
    (decf *sexpml-indent-level* 2))
  (emit-princ "</" name)
  (when (and (not empty?) *sexpml-indent*)
    (emit-indentation))  
  (emit-princ ">"))

(defun join-strings (list)
  (let* ((length (reduce #'+ list :key #'length))
         (result (make-string length)))
    (loop
       for string in list
       for start = 0 then end
       for end = (+ start (length string))
       while string
       do (replace result string :start1 start :end1 end)
       finally (return result))))

(defun fold-strings (list)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((strings '())
        (result '()))
    (dolist (object list)
      (typecase object
        (string (push object strings))
        (t (when strings
             (push (join-strings (nreverse strings)) result)
             (setf strings '()))
           (push object result))))
    (when strings
      (push (join-strings (nreverse strings)) result))
    (nreverse result)))

(defun compile-sexpml-forms (&optional (*sexpml-forms* *sexpml-forms*))
  (let ((forms (mapcar (lambda (form)
                         (if (stringp form)
                             `(write-string ,form *sexpml-output*)
                             form))
                       (fold-strings (reverse *sexpml-forms*)))))
    (if (> (length *sexpml-forms*) 1)
        (cons 'cl:progn forms)
        (first forms))))





(defmethod sexpml-form :around (name
                                &key &allow-other-keys)
  (declare (ignore name))
  (let ((*sexpml-indent-level* (if (boundp '*sexpml-indent-level*) 
                                       *sexpml-indent-level*
                                       0)))
   
    (if (boundp '*sexpml-forms*)
	(call-next-method)
	(let ((*sexpml-forms* nil))
	  (call-next-method)))))

(defun text-sexpml-form (escape contents)
  (loop :with *sexpml-forms*
     :for c :in contents
     :do 
     (emit-princ 
      (cond 
        ((stringp c) 
         (funcall escape c))
        ((sexpml-constant-p c)
         (funcall escape (princ-to-string c)))
        (t `(funcall ',escape (princ-to-string ,c)))))
     :finally (return (compile-sexpml-forms))))

(defgeneric sexpml-attributes-to-string (name attributes)
  (:method (name attributes)
    (declare (ignore name))
    (princ-attributes-to-string attributes :escape-string #'identity)))
  
(defmethod sexpml-form ((name (eql :text))
                        &key attributes
                          contents)
  (text-sexpml-form 'escape-string (append attributes contents)))

(defmethod sexpml-attributes-to-string ((name (eql :text)) attributes)
  (declare (ignore name))
  (with-output-to-string (s)
    (dolist (a attributes)
      (princ a s))))

(defmethod sexpml-form ((name (eql :unescaped))
                        &key attributes
                          contents)
  (text-sexpml-form 'identity (append attributes contents)))




(defun string-sexpml-tag-form (name attributes contents)
  (let (*sexpml-forms*)
    (emit-open-tag name :attributes attributes :empty? (not contents))
    `(prog2 ,(prog1 (compile-sexpml-forms)
                    (setf *sexpml-forms* nil))
         ,(prog1 (dolist (c contents (compile-sexpml-forms))
                   (emit-form c))
                 (setf *sexpml-forms* nil))
       ,(if contents 
            (prog2 (emit-close-tag name :empty? (not contents))
                (compile-sexpml-forms)
              (setf *sexpml-forms* nil))
            '(values)))))

(defmethod sexpml-form ((name string)
                              &key attributes contents)
  (string-sexpml-tag-form name attributes contents))

(defun uppercasep (tag)
  (every (lambda (char) 
           (or (and (alpha-char-p char) 
                    (upper-case-p char))
               (and (not (alpha-char-p char))
                    t)))
         (string tag)))

(defvar *symbol-home-package* nil)

(defmethod sexpml-form :around ((name symbol)
				&key attributes contents)
  ;; If there is a *SYMBOL-HOME-PACKAGE*, and there are no EQL methods
  ;; for this symbol, and there is an EQL method for the symbol
  ;; INTERN'd in the *SYMBOL-HOME-PACKAGE*, we go to that, otherwise
  ;; continue.
  (if (not *symbol-home-package*)
      (call-next-method)
      (let ((new-symbol (find-symbol (symbol-name name) *symbol-home-package*)))
	(if (and new-symbol
		 (not (find-method #'sexpml:sexpml-form '() `((eql ,name)) nil))
		 (find-method #'sexpml:sexpml-form '() `((eql ,new-symbol)) nil))
	    (sexpml-form new-symbol :attributes attributes :contents contents)
	    (call-next-method)))))

(defmethod sexpml-form ((name symbol)
                        &key attributes contents)
  "=> /tag-form/

If the symbol is entirely uppercase, then it is actually a lowercase tag.

For an uppercase tag, use a string"
  (sexpml-form (cond ((uppercasep name)
                      (string-downcase name))
                     (t (symbol-name name)))
               :attributes attributes
               :contents contents))

(defun find-compile-time-name (form)
  (when (listp form)
    (let* ((places '((cons . car)
                     (list . first)
                     (list* . first)
                     #+sbcl (sb-int:quasiquote . caar)))
           (op (first form))
           (place-fn (cdr (assoc op places))))
      (labels ((%find (&optional (form form))
                 (let* ((place (or (when place-fn
                                     (funcall place-fn (cdr form)))
                                   (cdr form)))
                        (thing 
                         (progn #+(or)(break "got it ~A" place)
                         (cond ((and (listp place)
                                     (eq (first place) 'cl:quote)
                                     (symbolp (second place)))
                                (second place))
                               ((symbolp place) place)))))
                   thing)))
        (%find)))))

(defun run-time-list-sexpml-tag-form (list &optional contents)
  (let* ((tag (gensym "sexpml-tag-"))
	 (name (gensym "sexpml-tag-name"))
	 ;; 
	 (compile-time-name (find-compile-time-name list))
	 (cname (gensym "sexpml-compile-time-name")))
                       
        ;;(break "~A ~A" (first (cdr list)) compile-time-name)

        `(let* ((,tag ,list)
                (,name (first ,tag))
                #+(or)(,name )
                (*sexpml-attributes* (rest ,tag))
                (,cname ',compile-time-name))
           (if (eq ,cname ,name)
               ,(sexpml-form compile-time-name 
                             :attributes `((sexpml-attributes-to-string 
                                            ,cname *sexpml-attributes*))
                             :contents contents)
               ,(string-sexpml-tag-form 
                 `(etypecase ,name
                     (symbol (cond ((uppercasep ,name)
                                    (string-downcase ,name))
                                   (t (symbol-name ,name))))
                     (string ,name))
                 `((sexpml-attributes-to-string ,name *sexpml-attributes*))
                 contents)))))


(defun sexpml-tag-name-p (thing)
  (typecase thing
    ((or string keyword) t)
    (symbol (or (not (fboundp thing))
		(get thing '<>)))
    (t nil)))

     
(defun list-sexpml-tag-form (list &optional contents)
  (let ((first (first list))) 
  (cond
    ;; quoted list
    ((eq first 'quote)
      (if (not (listp (second list)))
          ;; Not a list
          (sexpml-form (second list)
                       :attributes nil
                       :contents contents)
          ;; Is a list
          (sexpml-form (first (second list))
                       :attributes (mapcar 
                                    (lambda (a)
                                      (if (sexpml-constant-p a)
                                          a
                                          (list 'quote a)))
                                    (rest (second list)))
                       :contents contents)))
    ;; a compile time tag form
    ;; If the first item is a not a function, it is a tag name.
    ((sexpml-tag-name-p first)
     (sexpml-form first :attributes (rest list)
		  :contents contents))

    ;; Otherwise we are doing something crazy at run time... could
    ;; even be a backquote!
    
    (t (run-time-list-sexpml-tag-form list contents)))))  

(defmethod sexpml-form ((name list)
                        &key attributes
                          contents)
  (list-sexpml-tag-form (if attributes 
                            (append name attributes)
                            name) 
                        contents))

(defun <>-form (form &optional contents)
  (typecase form
    ((or string character)
     (assert (every (lambda (c) (or (stringp c) (characterp c))) contents)
             (contents)
             "Invalid <> : If the first argument is a string or a character, so the rest must be : ~A" contents)
     (sexpml-form :text :contents (list* form contents)))
    (t (sexpml-form form :contents contents))))

;;; * The <> Macro
 
                  
(defmacro <> (form &body contents)
  (<>-form form contents))
                
                

;; * Runtime :: ~sexpml-tag~

(defgeneric sexpml-output-tag (sexpml-output tag &optional contents-thunk))

#+(or)(defmethod sexpml-output-tag ((*sexpml-output* stream) tag &optional contents-thunk)
  (let ((tag (if (listp tag) tag (list tag))))
    (<> `(,@tag))))
  
  

;; Copyright (c) 2015-2016, Drew Crampsie
;; Copyright (c) 2003-2009, Dr. Edmund Weitz. 
;; Copyright (c) 2002-2005, Edward Marco Baringer

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
;;  - Neither the name of Drew Crampsie, Edward Marco Baringer, nor
;;    BESE, nor the names of its contributors may be used to endorse
;;    or promote products derived from this software without specific
;;    prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
