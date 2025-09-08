(defpackage :maxclaims/web-display/html-page
  (:use :cl :maxclaims/yaclml/tags)

  (:import-from :maxclaims/ecm-description
		#:ecm-attributes)
  (:import-from :drakma
		#:url-encode)
  (:import-from :maxclaims
		#:*debug-on-error*
		#:with-udb
		#:call-with-app-user)
  (:import-from :maxclaims/web-display/display
	#:get-app-user)
  (:import-from :maxclaims/hunchentoot
		#:define-easy-handler
		#:redirect)
  (:import-from :maxclaims/web-display/inline-edit
		#:<script-inline-edit>)
  (:export
	   #:call-with-html-page
	   #:with-user-html-page))
(in-package :maxclaims/web-display/html-page)

(defun <table-popup> ()
  (<:script
   (<:as-is
    '|// First, we take the header and remove it.
$('th[data-popout="true"]').remove();

// Now save the popouts
popout = $('td[data-popout="true"]');

// Now we need to create a TR ...  and add our TD to it that spans the
// number of TDs in the parent row
popout.map(function() {
 tr = $(this).closest('tr');
 length = tr.children('td').length;
 $(this).attr('colspan', length);
 new_tr = $('<tr/>');
 new_tr.append($(this));
 tr.after(new_tr);
 return length;});|)))

(defun call-with-html-page (function &key (title "ECM")
				       (refresh nil)
				       &allow-other-keys)

  (<:as-is (format nil "<!DOCTYPE html>~%"))
  (<:html
   :lang "en"
   (<:head
    (<:meta :charset "utf-8")
    (when refresh
      (<:meta :http-equiv "refresh"
	      :content refresh))
    (<:title (<:as-html title))
    (<:as-is '|<link href="https://maxcdn.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css" rel="stylesheet" integrity="sha384-4FeI0trTH/PCsLWrGCD1mScoFu9Jf2NdknFdFoJhXZFwsvzZ3Bo5sAh7+zL8Xgnd" crossorigin="anonymous">|)
    #+(or)    (<:link :href "http://50.7.166.105/bootstrap/css/bootstrap.css"
		      :rel "stylesheet")
    #+(or)    (<:link :href "http://50.7.166.105/bootstrap/css/bootstrap-responsive.css"
		      :rel "stylesheet")
    (<:link :href "https://cdnjs.cloudflare.com/ajax/libs/smalot-bootstrap-datetimepicker/2.3.10/css/bootstrap-datetimepicker.css"
	    :rel "stylesheet")
    (<:script :src "https://ajax.googleapis.com/ajax/libs/jquery/1.8.3/jquery.min.js")

    #+(or)    (<:script :src "http://50.7.166.105/bootstrap/js/bootstrap.js")
    (<:as-is '|<script src="https://maxcdn.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js" integrity="sha384-vOWIrgFbxIPzY09VArRHMsxned7WiY6hzIPtAIIeTFuii9y3Cr6HE6fcHXy5CFhc" crossorigin="anonymous"></script>|)
    #+(or)   (<:script :src "http://50.7.166.105/js/json2.js")
    (<:script :src "https://cdnjs.cloudflare.com/ajax/libs/smalot-bootstrap-datetimepicker/2.3.10/js/bootstrap-datetimepicker.js")

    (<:as-is "
      <!-- HTML5 shim, for IE6-8 support of HTML5 elements -->
      <!--[if lt IE 9]>
       <script src=\"http://html5shim.googlecode.com/svn/trunk/html5.js\"></script>
      <![endif]-->

")

    (<:style "
html, body { height: 100% }
:checked + span {
    font-weight: bold;
    font-size:120%;
    border:1px dashed grey;
    padding:0.5em;
    margin:0.5em;
    background-color:#F3F3F3;
}

#myModal {
top:50%;
right:50%;
position: absolute;
outline: none;
}

/*
* Placeholders consistency
* */
input::-webkit-input-placeholder, textarea::-webkit-input-placeholder {
color: grey;
}
input:-moz-placeholder, textarea:-moz-placeholder {
color: grey;
}
input:-ms-input-placeholder, textarea:-ms-input-placeholder {
color: grey;
}

input.input-required::-webkit-input-placeholder , textarea.input-required::-webkit-input-placeholder {
color: #B94A48;
}
input.input-required:-moz-placeholder, textarea.input-required:-moz-placeholder {
color: #B94A48;
}
input.input-required:-ms-input-placeholder, textarea.input-required:-ms-input-placeholder {
color: #B94A48;
}
     "
	     ))

   (<:body ;;"HERE"
    (<:div
     :class "body"
     (catch 'error
       (handler-bind ((error (lambda (c)
			       (unless *debug-on-error*
				 (throw 'error
				   (<:div
				    :class "text-error"
				    (<:h1 (<:as-html "Got Error :"))
				    (<:pre
				     (<:as-html (princ-to-string c)))
            (<:as-is`` "<button onclick=\"history.go(-2)\">&lt; Go Back To Edit</button>")

            ))))))
	 #+(or)(<:as-is "This is where the render function is called")
	 (funcall function))))

    (<table-popup>)
    (<:script
     (<:as-is
      "$(document).ready(function () {
$(\".form_datetime\").datetimepicker({
        format: \"yyyy-mm-dd hh:ii:ss ]Z[\",
        autoclose: true
    })
});"))


    (<:script
     (<:as-is
      "$(document).ready(function () {
$(\".form_date\").datetimepicker({
        format: \"yyyy-MM-dd\",
        minView: \"month\",
        autoclose: true
    });
});"))
    (<:as-is
     '#:|<div id="myModal" class="modal hide fade" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">

  <div class="modal-body">
    <p>One fine body…</p>
  </div>
      <button class="btn btn-primary">Save changes</button>
  </div>
</div>|)
    (<script-inline-edit>))))

(defun call-with-user-html-page (fn &rest html-page-args
				 &key uri-path
				   (refresh nil)
				   &allow-other-keys)
  (declare (ignorable refresh uri-path))
  (let ((app-user (get-app-user)))
    (if app-user
	(call-with-app-user
	 app-user
	 (lambda ()
	   (apply #'call-with-html-page
		  fn html-page-args)))
	(redirect
	 (format nil "/ecm/login?q=~A"
		 (drakma:url-encode (hunchentoot:request-uri*) :ascii))))))

(defmacro with-user-html-page ((&rest args)			       &body body)
  `(call-with-user-html-page
    (lambda () ,@body)
    ,@args))
