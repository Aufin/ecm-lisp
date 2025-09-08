(defpackage :ecm/llyods-report
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{)
  (:import-from :ecm/autocomplete
                #:js/find-autocomplete
                 #:<autocomplete-style>)
  (:import-from :ecm/find))
(in-package :ecm/llyods-report)

(defun <report-form> ()
  (<> '(form :method "POST" :action "/ecm/create?create[type]=spreadsheet-llyods-bordereau&access[read-only]=false")
    (<> '(input :name "present[CONTRACT]"
          :value "1969"
          :id "contracts"
          :type "hidden"))
    (<> 'h3 (<> "Start Date"))
    (<> '(input :name "present[START-DATE]"
          :class "datepicker"
          :id "start-date"
          :value "2016-01-01"))
    (<> 'h3 (<> "End Date"))
    (<> '(input :name "present[END-DATE]"
          :class "datepicker"
          :value "2016-01-31"))
    (<> '(input :name "present[SPREADSHEET-TYPE]"
          :value "Gnumeric_Excel:xlsx2"
          :type "hidden"))
    (<> 'hr)
    (<> '(button :type "submit" :class "btn btn-success")
      (<> "go"))
    (<> 'html5:script
      (ps:ps
        ($ (lambda ()
             (ps:chain ($ ".datepicker")
                       (datepicker))
             (ps:chain ($ ".datepicker")
                       (datepicker
                        "option" "dateFormat" "yy-mm-dd"))
             (let ((date (ps:new (|Date|))))
               (alert date.month)
               (if (= date.month 0)
                   (ps:setf date.month 11)
                   (ps:decf date.month))
               (ps:chain ($ "#start-date")
                       (datepicker "setDate" date)))))))))

(defun find-contract-object (&optional (term 'request.term))
  `({ 
     :_type "contract"
     :where ({ :contract_number ({ "$ilike" (concatenate 'string "%" ,term "%") })})
     :order_by (ps:array
                 ({ "contract_number" ({"$desc" ({"$eq" ,term })})}) 
                 )
    
    }))

(hunchentoot:define-easy-handler (llyods-report-handler
                                  :uri "/ecm/report/llyods")
    ()
  (<> '(ecm/ml:page :title "ECM: Llyods Report")
    (<> 'html5:h1 "Lloyds Claims Reporting Bordereau")
    (<> '(html5:div :class "center-block" :width "100%")
      "This is a Test"
      (<autocomplete-style>)
    (<> :unescaped '#:|

  <script>

  $(function() {|
        (js/find-autocomplete (find-contract-object)
                              :selector "#contract"
                              )
        '#:|
   });

  </script>

 <div class="ui-widget">
  <label for="contract">Contract Numbers: </label>
  <input id="contract">
</div> 
   |)
    (<> '(html5:div :id "listed-contracts")
      " ")
    (<report-form>))))
        
    
       
       
        
                   
           
                                       
    
    
    

