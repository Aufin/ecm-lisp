(uiop:define-package :ecm/ui/corpus
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.| #:$.)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/ui/utility
		#:cat #:<link-to-viewer>
		#:<item>)
  (:import-from :ecm/user)
  (:import-from :ecm/ui/autocomplete)
  (:import-from :ecm/json
		#:getjso)
;  (:import-from :ecm/ui/navbar)
  (:export #:<corpus>
	   #:<corpus-input>
	   #:<corpus-create>
	   #:corpus-name))

(in-package :ecm/ui/corpus)

(defun corpus-name (corpus)
  (let* ((first-name (getjso "first_name" corpus))
	 (last-name (getjso "last_name" corpus))
	 (company-name (getjso "company_name" corpus))
	 (province (getjso "province" corpus))
	 (short-name (and province (getjso "short_name" province))))
    (with-output-to-string (s)
      (when first-name (princ first-name s))
      (when last-name
	(princ #\Space s)
	(princ last-name s))
      (when (and (or first-name last-name)
		 company-name)
	(princ ", " s))
      (when company-name (princ company-name s))
      (when (and (or first-name last-name company-name)
		 short-name)
	(princ ", " s))
      (when short-name (princ short-name s)))))

(defun <corpus-input> (&optional selected &key (name "corpus-id")
					    (prefix ""))

  (<> (style)
    "
.ui-autocomplete {
  max-height: 200px;
  overflow-y: auto;
  /* prevent horizontal scrollbar */
  overflow-x: hidden;
  /* add padding to account for vertical scrollbar */
  position : relative;
  top: -0.5rem ;left:0.5rem;
        } ")
  (let ((hidden (symbol-name (gensym "hiddenCorpus")))
	      (hidden-cancel (symbol-name (gensym "hiddenCorpusCancel")))
	      (find (symbol-name (gensym "findCorpus")))
	      (find-enc (symbol-name (gensym "findCorpusEnc")))
	      (selected-cancel (symbol-name (gensym "SelectedCancel")))
	      (selected-corpus (symbol-name (gensym "SelectedCorpus")))
	      (corpus-close (symbol-name (gensym "corpusClose"))))
    (<> (div :class "ui-widget")
      (<> (div :id find-enc)
	      (<> (input :id find
		               :class "form-control"
		               :placeholder (if selected (getjso "full_name" selected))))
	      (when selected
	        (<> (a :href "#" :id selected-cancel)
	          "cancel")))
      (<> (input :type "hidden"
		             :id hidden
		             :name name
		             :value (if selected (getjso "_id" selected))))
      (<> (input :type "hidden"
		             :id hidden-cancel
		             :value (if selected (getjso "_id" selected))))
      (<> (div :id selected-corpus)
	      (when selected
	        (<corpus> selected)
	        (<> (div :class "corpus-delete close" :style "margin-left:50%"
		               :id corpus-close)
	          "X")))
      (let ((string (format nil "~{~A~}"
			                      (mapcar #'princ-to-string (list*
						                                           '|$(function() {
$("#| find '|").autocomplete({
    scroll:true,
    appendTo : '#| find-enc'|',
    source: function( request, response ) {
        $.getJSON( "/ecm/corpus/search", request, function( data, status, xhr ) {
          var create = [{"_type":"corpus_summary","_id":"","full_name":"Create New", "company_name":"Create New"}];
          response(create.concat(data));
        });
      }, 
    select: function(e,ui) {
        $("#| hidden '|").val(ui.item._id);
        $("#| find '|").attr("placeholder", ui.item.full_name);
        $("#| find-enc '|").hide();
        if (ui.item._id == '') {
            
             $.get("/ecm/corpus/create/inline?prefix=| prefix '|", 
                    function(data) { $("#| selected-corpus '|").html('<div class="corpus-delete close" style="margin-left:50%" id="| corpus-close '|">X</div> <br>') 
                                         .append(data) ;
           $("#| selected-corpus '|").show();

                            $("#| corpus-close '|").click(function() {
                               $("#| selected-corpus '|").hide();
                               $("#| find-enc '|").show();
                               $("#| hidden '|").val("");
                               $("#| find '|").attr("placeholder", "");
                              });
            });
       }  else {
             $.get("/ecm/corpus/" + ui.item._id + "/inline", 
                    function(data) { $("#| selected-corpus '|").html(data) 
                                         .append('<div class="corpus-delete close" style="margin-left:50%" id="|corpus-close|">X</div>') ;
           $("#| selected-corpus '|").show();
                            $("#|corpus-close|").click(function() {
                               $("#| selected-corpus '|").hide();
                               $("#| hidden '|").val("");
                               $("#| find-enc '|").show();
                               $("#| find '|").attr("placeholder", "");
                              });
            });
         };

      }})
   .autocomplete( "instance" )._renderItem = function( ul, item ) {
      return $( "<li>" )
        .append(renderObject(item))
        .appendTo( ul );
    };


});|
			       
						                                           (when selected
							                                           (list '|$(function() {
	   $("#| hidden '|").val(|(getjso "_id" selected)'|);
           $("#| find-enc '|").hide();
           $("#| corpus-close '|").click(function() {
                               $("#| selected-corpus '|").hide();
                               $("#| hidden '|").val("");
                               $("#| find-enc '|").show();
                               $("#| find '|").attr("placeholder", "");
                              });

           $("#| selected-cancel '|").click(function() {
                               $("#| selected-corpus '|").show();
                               $("#| hidden '|").val($("#| hidden-cancel '|").val());
                               $("#| find-enc '|").hide();
                               $("#| find '|").attr("placeholder", "");
                              });

          });|)
							                                           ))))))
                                        ;(break "~A" string)
	      (<> (script)
	        (ps:ps* (ecm/ui/autocomplete:render-object))
	        string
	        )))))
  

(defun <corpus> (corpus)
  (<> 'b
    (<link-to-viewer> ("corpus" (getjso "_id" corpus))
      (<> :text (getjso "full_name" corpus))))
  (let ((province (ecm/json:getjso* "province.short_name" corpus)))
    (when province 
      (<> :unescaped "&nbsp;")
      (<> (div :class "text-nowrap"
	       :style "display:inline-block")    
	(<> '(html5:small :class "text-muted")      
	  (<> :text " " province))))))

(defun <corpus-create> (&key first-name last-name company-name
			  birth-date
			  address-line-1 address-line-2
			  city province-id postal-code
			  email-address home-phone work-phone fax cell-phone
			  (name-prefix ""))
  
  (<> (div :class "container")
    (<> (div :class "row")
      (<> (div :class "col-md-2")
	(<> (h5) (<> (:text "First Name:"))))
      (<> (div :class "col-md-4")
	(<> (input :type "text" :class "form-control"
		   :name (cat name-prefix "first-name") :value (or first-name ""))))
      (<> (div :class "col-md-2")
	(<> (h5) (<> (:text "Last Name:"))))
      (<> (div :class "col-md-4")
	(<> (input :type "text" :class "form-control"
		   :name (cat name-prefix "last-name") :value (or last-name "")))))
    (<> (div :class "row")
      (<> (div :class "col-md-3 offset-md-2")
	(<> (h5) (<> (:text "Company Name:"))))
      (<> (div :class "col-md-7")
	(<> (input :type "text" :class "form-control"
		   :name (cat name-prefix "company-name") :value (or company-name "")))))
    (<> (div :class "row")
      (<> (div :class "col-md-2")
	(<> (h5) (<> (:text "Address 1"))))
      (<> (div :class "col-md-4")
	(<> (input :type "text" :class "form-control"
		   :name (cat name-prefix "address-line-1")
		   :value (or address-line-1 ""))))
      (<> (div :class "col-md-2")
	(<> (h5) (<> (:text "Address 2"))))
      (<> (div :class "col-md-4")
	(<> (input :type "text" :class "form-control"
		   :name (cat name-prefix "address-line-2") :value (or address-line-2 "")))))
    (<> (div :class "row")
      (<> (div :class "col-md-2")
	(<> (h5) (<> (:text "City"))))
      (<> (div :class "col-md-4")
	(<> (input :type "text" :class "form-control"
		   :name (cat name-prefix "city"):value (or city ""))))
      (<> (div :class "col-md-2")
	(<> (h5) (<> (:text "Province:"))))
      (<> (div :class "col-md-4")
	(<province-select> :selected province-id
			   :prefix name-prefix)))
    (<> (div :class "row")
      (<> (div :class "col-md-2")
	(<> (h5) (<> (:text "Postal Code:"))))
      (<> (div :class "col-md-4")
	(<> (input :type "text" :class "form-control"
		   :name (cat name-prefix "postal-code") :value (or postal-code ""))))
      (<> (div :class "col-md-2")
	(<> (h5) (<> (:text "Email Address:"))))
      (<> (div :class "col-md-4")
	(<> (input :type "text" :class "form-control"
		   :name (cat name-prefix "email-address") :value (or email-address "")))))
    (<> (div :class "row")
      (<> (div :class "col-md-3 offset-md-2")
	(<> (h5) (<> (:text "Birth Date:"))))
      (<> (div :class "col-md-4")
	(<> (input :type "text" :class "form-control birthdatepicker"
		   :name (cat name-prefix "birth-date") :value (or birth-date "")))))
        (<> (div :class "row")
      (<> (div :class "col-md-2")
	(<> (h5) (<> (:text "Home Phone:"))))
      (<> (div :class "col-md-4")
	(<> (input :type "text" :class "form-control"
		   :name (cat name-prefix "home-phone") :value (or home-phone ""))))
      (<> (div :class "col-md-2")
	(<> (h5) (<> (:text "Work Phone:"))))
      (<> (div :class "col-md-4")
	(<> (input :type "text" :class "form-control"
		   :name (cat name-prefix "work-phone") :value (or work-phone "")))))
	    (<> (div :class "row")
      (<> (div :class "col-md-2")
	(<> (h5) (<> (:text "Cell Phone:"))))
      (<> (div :class "col-md-4")
	(<> (input :type "text" :class "form-control"
		   :name (cat name-prefix "cell-phone")
		   :value (or cell-phone ""))))
      (<> (div :class "col-md-2")
	(<> (h5) (<> (:text "Fax:"))))
      (<> (div :class "col-md-4")
	(<> (input :type "text" :class "form-control"
		   :name (cat name-prefix "fax") :value (or fax ""))))))
  (<> 'html5:script
    (ps:ps
      ($(lambda ()
	  (ps:chain
	   ($ ".birthdatepicker")
	   (datepicker
	    ({} "changeMonth" t
		"changeYear" t
		"yearRange" "1900:2018"
		"orientation" "bottom"
		"timeInput" :false
		"dateFormat" "yy-mm-dd"
		"showButtonPanel" t
		"maxDate"  0
		"controlType" "select"
		"oneLine" t))))))))

(defun <province-select> (&key selected prefix)
  (<> (html5:select :name (cat prefix "province-state")
		    :class "form-control")
    (<> (html5:option :value) "")
    (dolist (p (ecm/entity/corpus:list-provinces))
      (<> (html5:option :value (getjso "province_state_id" p)
			(when(equalp (getjso "province_state_id" p) selected)
			  (list :selected t)))
	(<> :text (getjso "long_name" p))))))
  
(defun <edit-corpus> (corpus )
  (<> 'b
    (<link-to-viewer> ("corpus" (getjso "_id" corpus))
      (<> :text (getjso "corpus_number" corpus))))
  (<item> " insured" (getjso "insured" corpus))
  (<> (div :class "text-nowrap"
	   :style "display:inline-block")
    (<> '(html5:small :class "text-muted")
      (<> :unescaped "&nbsp;" " effective" "&nbsp;"))
    (<> :text (getjso "effective_date" corpus)))
  (<> (div :class "text-nowrap"
	   :style "display:inline-block")    
    (<> '(html5:small :class "text-muted")
      (<> :unescaped "&nbsp;" " expiry" "&nbsp;"))
    (<> :text (getjso "expiry_date" corpus))))



