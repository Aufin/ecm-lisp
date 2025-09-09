(defpackage :sexpml/html5
  (:use :cl)
  (:nicknames :html5)
  (:import-from :sexpml
		#:sexpml-form)
  (:export #:<>
	   #:&
	   #:doctype 
	   #:html
	   
	   ;; Document metadata
           #:head
           #:link
           #:title
	   #:body
	   #:script
	   #:style
	   #:nav
	   #:button
	   #:i
	  
	   ;; Grouping content
	   #:p #:hr #:pre #:blockquote
	   #:ol #:ul #:li #:dl #:dt #:dd
	   #:figure #:figcaption #:main #:div #:code

	   ;; Heading content 
           #:h1 #:h2 #:h3 #:h4 #:h5 #:h6

	   ;; Phrasing content

	   #:a

	   ;; abbr area (if it is a descendant of a map element) audio b bdi bdo br button canvas cite code data datalist del dfn em embed i

	   #:iframe
	   
	   ;img input ins kbd keygen label map mark math meter noscript object output picture progress q ruby s samp script select
	   
	   #:small
	   #:span #:strong
	   ;; sub sup svg template textarea time u var video wbr text

	   ;; - Tabular data

	   #:table
	   #:caption
	   #:thead
	   #:tbody
	   #:tr
	   #:th
	   #:td

	   ;; - FORMS

	   #:form
	   #:option
	   #:input
	   #:button
	   #:select
	   #:textarea
	   #:fieldset
	   #:output	
	   #:object	
	   #:meter	
	   #:progress
	   #:label
	   #:img)
	   
  (:documentation "HTML5 tags for SEXPML 
 - Symbolic Expression eXtensible Programmable Markup Language -
 - http://www.w3.org/TR/html51/"))
(in-package :sexpml/html5)

(defmacro <> (form &body contents)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (let ((sexpml:*symbol-home-package* :sexpml/html5))
      (sexpml:<>-form form contents))))

;; * Start


(defun sexpml-form-with-end-tag (name attributes contents)
  (sexpml:sexpml-form name :attributes attributes
                      :contents 
                      (if contents 
                          contents
                          (list ""))))

(defmacro deftag (name &key (indent t) (end-tag-required t))
  (let ((string (string-downcase (string name))))
    `(defmethod sexpml-form ((tag (eql ',name))
			     &key attributes contents)
       (let (,@(unless indent
		 '((sexpml:*sexpml-indent* nil))))
	 ,(if end-tag-required
	      `(sexpml-form-with-end-tag
		,string attributes contents)
	      `(sexpml-form ,string :attributes attributes :contents contents))))))
  

;; * Doctype

(defmethod sexpml-form ((tag (eql 'doctype))
		     &key attributes contents)
  "=> /form/ 

*Description:*

A DOCTYPE is a required preamble.

DOCTYPEs are required for legacy reasons. When omitted, browsers tend
to use a different rendering mode that is incompatible with some
specifications. Including the DOCTYPE in a document ensures that the
browser makes a best-effort attempt at following the relevant
specifications.

A DOCTYPE must consist of [...]. In other words, <!DOCTYPE html>,
case-insensitively.

It is suggested that newlines be inserted after the DOCTYPE [...]."
  (declare (ignore attributes contents))
  `(<> :unescaped 
     "<!DOCTYPE html>" (string #\Newline)))
 
;; * HTML element

(defmethod sexpml-form ((tag (eql 'html))
		     &key attributes contents)
  "http://www.w3.org/TR/html51/semantics.html#the-html-element"
  (sexpml:sexpml-attributes-bind ((doctype nil)) attributes
;    (break "att: ~A" attributes)
    (let ((dt (gensym)))
      `(progn ,@ (when doctype
		   `((let ((,dt ,doctype))
		       (when ,dt
			 (<> '(doctype))))))
	 ,(sexpml-form "html" 
		    :attrbutes attributes
		    :contents contents)))))

;; * Document metadata

;; ** The head element
(defmethod sexpml-form ((tag (eql 'head))
		     &key attributes contents)
  (sexpml-form "head" 
	    :attributes attributes
	    :contents contents))

;; ** The title element

(defmethod sexpml-form ((tag (eql 'title))
		     &key attributes contents)
  "Content model:
Text that is not inter-element whitespace.
Tag omission in text/html:
Neither tag is omissible."

  (sexpml-form-with-end-tag 
   "title" attributes 
   (if (every 'stringp contents)
       (list (sexpml-form :text :contents contents))
       contents)))
;; * Sections

;; ** The body element

(defmethod sexpml-form ((tag (eql 'body))
		     &key attributes contents)
  "http://www.w3.org/TR/2014/WD-html51-20140617/sections.html#the-body-element"
  (sexpml-form "body" 
	    :attributes attributes
	    :contents contents))

;; ** The nav element

(deftag nav)


;; ** Heading content : The h1, h2, h3, h4, h5, and h6 elements

(deftag h1 :indent nil)
(deftag h2 :indent nil)
(deftag h3 :indent nil)
(deftag h4 :indent nil)
(deftag h5 :indent nil)
(deftag h6 :indent nil)

;; * 4.7. Embedded content

;; ** 4.7.5. The img element

;; - Tag omission in text/html ::  No end tag.

;; https://www.w3.org/TR/html51/single-page.html#the-img-element

(deftag img :indent nil :end-tag-required nil)


;; *  Phrasing content

;; Phrasing content is the text of the document, as well as elements
;; that mark up that text at the intra-paragraph level. Runs of
;; phrasing content form paragraphs.

;;  a abbr area (if it is a descendant of a map element) audio b bdi
;; bdo br button canvas cite code data datalist del dfn em embed i
;; iframe img input ins kbd keygen label map mark math meter noscript
;; object output picture progress q ruby s samp script select small
;; span strong sub sup svg template textarea time u var video wbr text

(deftag a :indent nil)
(deftag small :indent nil)
(deftag strong :indent nil)




;; * Grouping content
;; https://www.w3.org/TR/html51/single-page.html#grouping-content

;; ** The p element

(deftag p)

;; ** The hr element


(deftag hr)
;; 4.4.3The pre element

(deftag pre)
;; 4.4.4The blockquote element

(deftag blockquote)
;; 4.4.5The ol element
;; 4.4.6The ul element
;; 4.4.7The li element
;; 4.4.8The dl element
;; 4.4.9The dt element
;; 4.4.10The dd element
;; 4.4.11The figure element
;; 4.4.12The figcaption element
;; 4.4.13The main element
;; 4.4.14The div element
;; ** The ul element

(deftag ul)

;; ** The li element

(deftag li)

;; ** The div element

(deftag div :end-tag-required t)

;; * Tabular data

(deftag table)
(deftag caption)
(deftag thead)
(deftag tbody)
(deftag tr)
(deftag th)
(deftag td)


;; * Forms

;; "A form is a component of a Web page that has form controls, such
;; as text fields, buttons, checkboxes, range controls, or color
;; pickers. A user can interact with such a form, providing data that
;; can then be sent to the server for further processing (e.g.,
;; returning the results of a search or calculation). No client-side
;; scripting is needed in many cases, though an API is available so
;; that scripts can augment the user experience or use forms for
;; purposes other than submitting data to a server."

;;  -- https://www.w3.org/TR/html51/single-page.html#sec-forms

(deftag form)

(deftag input :indent nil :end-tag-required nil)

(deftag button :indent t :end-tag-required t)

(deftag select)
(deftag option :end-tag-required t)
(deftag textarea :end-tag-required t :indent t)
(deftag fieldset)
(deftag output)
(deftag object)
(deftag meter)
(deftag progress)
(deftag label)




;; * &rest


 

(defmethod sexpml-form ((tag (eql 'iframe))
		     &key attributes contents)
  "Tag omission in text/html:
Neither tag is omissible.

The HTML parser treats markup inside iframe elements as text.
-- http://www.w3.org/TR/html51/semantics.html#iframe-content-model
"

  (sexpml-form-with-end-tag "iframe" attributes contents))

(defmethod sexpml-form ((tag (eql 'script))
		     &key attributes contents)
    "=> /form/

http://www.w3.org/TR/html51/scripting-1.html#the-script-element

*Description*

 The script element allows authors to include dynamic script and data
 blocks in their documents. The element does not represent content for
 the user.

*Content model:*

 If there is no src attribute, depends on the value of the type
 attribute, but must match script content restrictions.

 If there is a src attribute, the element must be either empty or
 contain only script documentation that also matches script content
 restrictions.

*Content Restrictions:* 

#+BEGIN_QUOTE 
*Note:* The easiest and safest way to avoid the rather strange
 restrictions described in this section is to always escape \"<!--\" as
 \"<\!--\", \"<script\" as \"<\script\", and \"</script\" as
 \"<\/script\" when these sequences appear in literals in scripts
 (e.g. in strings, regular expressions, or comments), and to avoid
 writing code that uses such constructs in expressions. Doing so avoids
 the pitfalls that the restrictions in this section are prone to
 triggering: namely, that, for historical reasons, parsing of script
 blocks in HTML is a strange and exotic practice that acts
 unintuitively in the face of these sequences.

-- http://www.w3.org/TR/html51/scripting-1.html
#+END_QUOTE 

*Tag omission in text/html:*
  Neither tag is omissible"

  (let ((contents 
	 (if contents
	     (list (sexpml-form 
		    :unescaped
		    :contents contents))
	     ;; *Tag omission in text/html:*
	     ;; Neither tag is omissible
	     (list ""))))

  (sexpml-form 
   "script"
   :attributes attributes
   :contents contents)))

(defmethod sexpml-form ((tag (eql 'style))
		     &key attributes contents)

  (let ((contents 
	 (if contents
	     (list (sexpml-form 
		    :unescaped
		    :contents contents))
	     ;; *Tag omission in text/html:*
	     ;; Neither tag is omissible
	     (list ""))))

  (sexpml-form 
   "style"
   :attributes attributes
   :contents contents)))


(defmethod sexpml-form ((tag (eql 'link))
                        &key &allow-other-keys)
  (call-next-method))
