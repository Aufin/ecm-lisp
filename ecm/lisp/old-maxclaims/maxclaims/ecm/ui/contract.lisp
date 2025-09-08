(defpackage :ecm/ui/contract
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/entity/corpus)
  (:import-from :ecm/entity/contract)
  (:import-from :ecm/ps #:{} #:|.| #:$.)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/ui/utility
		#:cat #:<link-to-viewer>
		#:<item>)
  (:import-from :ecm/user)
  (:import-from :ecm/json
		#:getjso)
  (:import-from :ecm/ui/navbar)
  (:import-from :ecm/ui/autocomplete)
  (:import-from :ecm/local-time)
  
  (:export #:<select-contracts>
	   #:<contract>
	   #:ps/find-contract-object
	   #:<contract-autocomplete>
	   #:<contract-display>))
(in-package :ecm/ui/contract)

(defun ps/find-contract-object (&optional (term 'request.term))
  `({} 
     :_type "contract"
     :limit 10
     :where ({} "$or"
		({} :contract_number 
		    ({} "$ilike" (concatenate 'string ,term "%"))
		    :contract_number 
		    ({} "$ilike" (concatenate 'string "%",term "%"))))
     :order_by (ps:array
		({} "contract_id" ({} "$desc" t)))))

(defun <contract-display> (contract
			   &key (style "display:inline-block;width:80%"))
  (<> (div :style style
	   :id "contractDisplay")
    (<> (div :class "close"
	       :style "float:right"
		:id "contractClose")
	 (<> "x"))
      (<> (span)
	(<> (h3) (<> :text (getjso "contract_number" contract )))
	(loop :for item
	   :in '("effective_date"
		 "expiry_date"
		 "agency"
		 "syndicate"
		 "london_broker")
	   :do 
	   (let ((value (getjso item contract)))
	     (when value
	       (<> (span :style "display:inline-block;margin-right:1em")
		 (<> (small :class "text-muted")
		   (<> :text item " "))
		 (<> :text
		   (if (stringp value)
		     value
		     (ecm/entity/corpus:corpus-name-as-string value))))))))
      
      (<> (hr)))

  (<> (script)
    (ps:ps
      ($. "#contractClose" (click (lambda ()
				    ($. "#contractID" (val nil))
				    ($. "#contractDisplay" (remove))
				    ($. "#contractInvalid" (remove))
				    ($. "#contractHr" (remove))
				    ($. "#contractHide" (show)))))
      ($ (lambda () ($. "#contractHide" (hide)))))))

(defun <contract-autocomplete>
    (&key (find-contract-object
	         (ps/find-contract-object))
       (selector "#contract")
       (display-style "display:inline-block; width:80%;")
       (width "80%")
       (select-parenscript
	      `(lambda (event ui)
	         ($. "#contractInvalid" (remove))
	         ($. "#contractHide" (hide))
	         ($. "#contractID" (val ui.item._id))
	         ($. "#contractShow"
	             (width ,width)
	             (append
		            ($. "<div>"
		                (attr "id" "contractDisplay")
		                (attr "style" ,display-style)
                    (attr "class" "w-100")
		                (append 
		                 (ps:chain ($"<div>")
			                         (attr "class" "col-xs-11")
			                         (append (render-object ui.item))))
		                (append (ps:chain ($"<div>")
				                              (attr "class" "col-xs-1 close")
				                              (attr "id" "contractClose")
				                              (append "x")))))
	             (append ($. "<hr>"
			                     (attr "id" "contractHr"))))

	         ($. "#contractClose" (click (lambda ()
					                               ($. "#contractID" (val nil))
					                               ($. "#contractDisplay" (remove))
					                               ($. "#contractHr" (remove))
				                                 ($. "#contractHide" (show))
                                         ($. ".ui-autocomplete" (hide)))))))
       (autocomplete-output "#contractAutoAppend")
       (minimum-length 2))
  (ecm/ui/autocomplete:<autocomplete-style>)
  (<> (style)
    ".close:hover { text-decoration:underline };"
    )
  (<> :unescaped '#:|
<script>
  $(function() {|
    (ecm/ui/autocomplete:js/find-autocomplete
     find-contract-object
     :append-to "#contractAutoAppend"
     :selector  selector
     :select select-parenscript
     :min-length minimum-length
   
     )
    '#:|
   });
</script>|))
  

(defun <select-contracts> (&key selected (name "contract-id")
			     (allow-none nil))
  (<> (select :name name
	      :class "form-control")
    (when allow-none
      (<> (option) ""))
    (dolist (c (ecm/entity/contract:list-contracts))
      (let ((id (getjso "_id" c))
	    (cn (getjso "contract_number" c))
	    (ef (ecm/json:null->nil (getjso "effective_date" c)))
	    (ex (ecm/json:null->nil (getjso "expiry_date" c)))
	    (a (ecm/json:null->nil (getjso "agency" c))))
	(<> (option :value id
		    (when (equalp id (and selected (getjso "_id" selected)))
		      (list :selected "selected")))
	  (<> :text cn)
	  (when a (<> :text " :: Agency - " (ecm/entity/corpus:corpus-name-as-string a)))
	  (when (or ef ex) (<> :text " :: "))
	  (when ef (<> :text " " ef))
	  (when ex (<> :text " to " ex)))))))
  

(defun <contract> (contract)
  (<> 'b
    (<link-to-viewer> ("contract" (getjso "_id" contract))
      (<> :text (getjso "contract_number" contract))))
  (<> " ")
  (<item> "effective date" (getjso "effective_date" contract))
  (<> " ")
  (<item> "expiry_date" (getjso "expiry_date" contract))
  (<> " ")
  (<item> "agency" (getjso "agency" contract))
  (<> " ")
  (<item> "syndicate" (getjso "syndicate" contract))
  (<item> "London Broker" (getjso "london_broker" contract))
  )


