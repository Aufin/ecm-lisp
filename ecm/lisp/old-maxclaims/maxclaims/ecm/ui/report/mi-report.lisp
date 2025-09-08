(defpackage :ecm/ui/report/mi
   (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.|)
  (:import-from :ecm/autocomplete
                #:js/find-autocomplete
                #:<autocomplete-style>)
  (:import-from :ecm/entity/syndicate 
                #:ps/find-syndicate-object))
(in-package :ecm/ui/report/mi)

(defun <mi-report-form> ()
  (<> '(form :method "POST" 
        :action "/ecm/report")
    (<> '(input :name "syndicate"
          :value "1969"
          :id "syndicate-input"
          :type "hidden"))
    (<> 'h3 (<> "Start Date"))
    (<> '(input :name "start-date"
          :class "datepicker"
          :id "start-date"
          :value "2016-01-01"))
    (<> 'h3 (<> "Report Period"))
    (<> '(input :name "interval"
          :value "1 Month"))
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
             (let* ((date (ps:new (|Date|)))
                    (month (|.| date (get-month))))
               (|.| date (set-month 
                          (if (= month 0) 
                              11
                              (ps:decf month))))
               (|.| date (set-date 1))
               (ps:chain ($ "#start-date")
                         (datepicker "setDate" date)))))))))
  
(hunchentoot:define-easy-handler (mi-report-handler
                                  :uri "/ecm/report/mi")
    ()
  (<> '(ecm/ml:page :title "MI Report :: ECM")
    (<> 'html5:h1 "MI Report")
    (<> '(html5:div :class "center-block" :width "100%")
      "This is a Test"
      (<autocomplete-style>)
    (<> :unescaped '#:|

  <script>

  $(function() {|
        (js/find-autocomplete 
         (ps/find-syndicate-object 'request.term)
         :selector "#syndicate"
         :min-length 1
         :select '(lambda (e ui)
                   (|.| ($ "#syndicate-input") (val ui.item.id))))
                    
                               
                   '#:|
   });

  </script>

 <div class="ui-widget">
  <label for="contract">Contract Numbers: </label>
  <input id="syndicate">
</div> 
   |)
    (<> '(html5:div :id "listed-contracts")
      " ")
        (<mi-report-form>))))

