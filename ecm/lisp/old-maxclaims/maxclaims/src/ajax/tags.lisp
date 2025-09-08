(in-package :ucw.js.tags)

;;; Yanked from ucw_ajax with some modifications
(deftag-macro <js:script (&attribute (compile-time-p nil) (toplevelp nil)
                                      &allow-other-attributes others
                                      &body body)
  "Insert a script tag compiling each expression in BODY with (js:js* ...).

When COMPILE-TIME-P is true the js:js* calls are made as part of the
macroexpansion, and the resulting strings are concatenated and emitted
as a single string constant at runtime.

Unless TOPLEVELP is true the body is wrapped in a JS closure, so it
has its own 'toplevel' bindings that are separated from the global
variables of the page (though they are shared between the expressions
in BODY).

If you want to define page-global variables then you must enable TOPLEVELP, but
then you should also be aware of how JS closures work. If unsure, simply avoid
installing event handlers from TOPLEVELP <ucw:script tags.

The most common pitfall is to think that in

 (<js:script :toplevelp t
  `(let ((var value))
     (lambda ()
       body))
  `(let ((var value))
    (lambda ()
      body2)))

BODY has its own binding of VAR, but in fact VAR is compiled into a page-global
variable. The JS closure, that references VAR, will see the current value of
the page-global binding of VAR as opposed to a copy of it made at the time the
closure was created.

Please note that in either case VAR is shared between BODY and BODY2."
  ;; was (unless toplevelp (setf body ...)) but I'm not sure that is
  ;; exactly legal (unless &body is exempted from the structure
  ;; sharing rules ala &rest)
  (let ((body (if toplevelp
		  body
		  (list ``((lambda ()
                          ,,@body))))))
    `(<:script :type "text/javascript"
	       ,@others
	       (<:as-is #\Newline "// <![CDATA[" #\Newline
			,(if compile-time-p
			     (iter (for expr in body)
				   (collect (js:js* (eval expr)) :into result)
				   (finally (return (apply #'concatenate 'string result))))
			     `(js:js* ,@body))
			#\Newline "// ]]>" #\Newline))))

;; Portions Copyright (c) 2003-2005 Edward Marco Baringer
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
