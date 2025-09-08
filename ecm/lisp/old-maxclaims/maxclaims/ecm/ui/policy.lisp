(uiop:define-package :ecm/ui/policy
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.| #:$.)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/ui/utility
		#:cat #:<link-to-viewer>
		#:<item>)
  (:import-from :ecm/user)
  (:import-from :ecm/json
		#:getjso)
;  (:import-from :ecm/ui/navbar)
  (:export #:<policy>
	   #:<policy-input>))

(in-package :ecm/ui/policy)

(defun <policy-input> (&optional selected)
  (<> (script)
    '|$(function() { 
$("#findPolicy").autocomplete({ 
    source: "/ecm/policy/search", 
    select: function(e,ui) {
        console.log(ui);
        $("#hiddenPolicy").val(ui.item._id);
        $("#findPolicy").attr("placeholder", ui.item.policy_number + " "
            + " insured " + ui.item.insured_name) ;

        $.get("/ecm/policy/" + ui.item._id + "/inline", 
              function(data) { $("#selectedPolicy").html(data); });
      }})
   .autocomplete( "instance" )._renderItem = function( ul, item ) {
      return $( "<li>" )
        .append( "<strong>" + item.policy_number + " </strong>"
                  + " insured <b>" + item.insured_name + "</b>"  + " effective " 
                  + "<b>" + item.effective_date + "</b>-<b>" + item.expiry_date +"</b>" )
        .appendTo( ul );
    };
});|)
  (<> (div :class "ui-widget")
    (<> (label :for "findPolicy")
      "Policy Number or Insured Name:")
    (<> (input :id "findPolicy"
	       :class "form-control"
	       :placeholder (if selected (getjso "policy_number" selected))))
    (<> (input :type "hidden"
	       :id "hiddenPolicy"
	       :name "policy-id"
	       :value (if selected (getjso "_id" selected)))))
  (<> (hr))
  (<> (div :id "selectedPolicy")
    (<policy> selected)))
  

(defun <policy> (policy)
  (<> 'b
    (<link-to-viewer> ("policy" (getjso "_id" policy))
      (<> :text (getjso "policy_number" policy))))
  (<item> " insured" (getjso "insured" policy))
  (<> (div :class "text-nowrap"
	   :style "display:inline-block")
    (<> '(html5:small :class "text-muted")
      (<> :unescaped "&nbsp;" " effective" "&nbsp;"))
    (<> :text (getjso "effective_date" policy)))
  (<> (div :class "text-nowrap"
	   :style "display:inline-block")    
    (<> '(html5:small :class "text-muted")
      (<> :unescaped "&nbsp;" " expiry" "&nbsp;"))
    (<> :text (getjso "expiry_date" policy))))

(defun <edit-policy> (policy)
  (<> 'b
    (<link-to-viewer> ("policy" (getjso "_id" policy))
      (<> :text (getjso "policy_number" policy))))
  (<item> " insured" (getjso "insured" policy))
  (<> (div :class "text-nowrap"
	   :style "display:inline-block")
    (<> '(html5:small :class "text-muted")
      (<> :unescaped "&nbsp;" " effective" "&nbsp;"))
    (<> :text (getjso "effective_date" policy)))
  (<> (div :class "text-nowrap"
	   :style "display:inline-block")    
    (<> '(html5:small :class "text-muted")
      (<> :unescaped "&nbsp;" " expiry" "&nbsp;"))
    (<> :text (getjso "expiry_date" policy))))



