(defpackage :maxclaims/entry-point/licence
  (:use :cl)
  (:import-from :maxclaims/ucw/application
		#:*ecm-application*)
  (:import-from :maxclaims/web-display/html-page
		#:call-with-html-page)
		
  (:import-from :ucw
		#:defentry-point))
  

(in-package :maxclaims/entry-point/licence)

(defentry-point "licence" (:application *ecm-application*
					:with-call/cc nil)
    ()
  (call-with-html-page 
   (lambda () 
     (<:pre 
      :class "lead"
      (<:as-html "The MIT License (MIT)
Copyright (c) 2013 Drew Crampsie

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the \"Software\"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
	    
  )")))))
  
