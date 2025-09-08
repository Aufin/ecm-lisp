(defpackage :ecm/ui/report/transaction-bordereau
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.|)
  (:import-from :ecm/ui/autocomplete
                #:js/find-autocomplete
                 #:<autocomplete-style>)
  (:import-from :ecm/api/find)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/user)
  (:import-from :ecm/report/transaction-bordereau
                :transaction-bordereau)
  (:import-from :ecm/ui/spreadsheet  #:<spreadsheet-type-select>))
(in-package  :ecm/ui/report/transaction-bordereau)

(defun <report-form> ()
  (<> '(form :method "POST" 
        :action "/ecm/report/transaction-bordereau"
        :target "report-frame")
    (<> '(div :class "row row-centered")
      (<> '(input :name "contract-numbers"
            :id "contracts"
            :type "hidden"))
      (<> '(div :class "col-md-4 col-centered")
        (<> '(div :class "row-centered")
          (<> 'h3 (<> "Start Date"))
          (<> '(input :name "start-date"
                :class "datepicker"
                :id "start-date"))))
      (<> '(div :class "col-md-4 col-centered")
        (<> '(div :class "row-centered")
          (<> 'h3 (<> "Period"))
          (<> '(input :name "interval"
                :value "1 Month"))))
      (<> '(input :name "present[SPREADSHEET-TYPE]"
            :value "Gnumeric_Excel:xlsx2"
            :type "hidden")))
    (<> '(div :class "row row-centered")
      (<> '(div :class "col-xs-8 col-centered")
        (<> '(div :class "row-centered")
      (<> 'br)
      (<> '(div :class "ui-widget")
        (<> '(label :for "contract")
          (<> 'h3 (<> "Contract Number(s)")))
        (<> 'br))
      (<> '(input :id "contract"))
      (<> '(div :id "listed-contracts")
            " "))))
    (<> '(div :class "row row-centered")
      (<> '(div :id "auto-append"
            :onload "this.width=$(\".container\").width();")))
    (<> '(div :class "row row-centered report")
      (<> 'br)
      (<> '(div :class "col-xs-6 col-centered")
        (<> '(span :id "ss-type-span"
              :class "row-centered"))
          (<> '(button :type "submit" :class "btn btn-success")
            (<> "Run Report"))
          (<> " as ")
          (<spreadsheet-type-select> 
           :name "spreadsheet-type"
           :class "spreadsheet-type-select"))))
      (<> 'html5:script
        (ps:ps
          ($ (lambda ()
               (|.| ($ ".report") (hide))
               (ps:chain ($ ".datepicker")
                         (datepicker))
               (ps:chain ($ ".datepicker")
                         (datepicker
                          "option" "dateFormat" "yy-mm-dd"))
               (let* ((date (ps:new (|Date|)))
                      (month (ps:chain date (get-month))))
                 (|.| date (set-month 
                            (if (= month 0) 
                                11
                                (ps:decf month))))
                 (|.| date (set-date 1))
                 (when (= "" (|.| ($ "#start-date") (val)))
                          (ps:chain ($ "#start-date")
                                    (datepicker "setDate" date)))))))))


    

(defun find-contract-object (&optional (term 'request.term))
  `({} 
     :_type "contract"
     :where ({} :contract_number 
                ({} "$ilike" (concatenate 'string "%" ,term "%")))
     :order_by (ps:array
                ({} "contract_number" ({} "$desc" ({} "$eq" ,term ))))))

(defun js/report-functions ()
  (ps:ps 
    (progn 
      (defun make-row (object &optional (type "<td>") 
                                (heading? ps:false))
        (let ((row ($ "<tr>")))
          ($.map object (lambda (i h)
                          (row.append 
                           (|.| ($ type) (html (if heading? h i))))))))

      (defun make-table (array) 
        (let ((head ($ "<thead>"))
              (body ($ "<tbody>"))
              (table ($ "<table>")))
          (head.append (make-row (aref array 0) "<th>" t))
          (table.append head)

          ($.map array (lambda (o) (body.append (make-row o))))
          (table.append body)
          table)))))
                

(defun <transaction-bordereau-page>  ()
  (<> '(ecm/ui/page:page :title "Transaction Bordereau : ECM")
        (<> 'style 
          (<> "/* centered columns styles */
.row-centered {
    text-align:center;
}
.col-centered {
    display:inline-block;
    float:none;
    /* reset the text-align */
    text-align:left;
    /* inline-block space fix */
    margin-right:-4px;
}"))
        (<> '(div :class "row row-centered")
          (<> '(html5:h1) "Transaction Bordereau")
          (<> '(div 
                :class "col-xs-6 col-centered row-centered")
            (<autocomplete-style>)
            (<> 'html5:script 
              (js/report-functions))
            (<> :unescaped '#:|
<script>
  $(function() {|
                (js/find-autocomplete (find-contract-object)
                                      :selector "#contract"
                                      :position '({} 
                                                  :of "#auto-append"
                                                  :my "center top" 
                                                  :at "center bottom"))
                '#:|
   });
</script>|)))
        
        (<report-form>)
        (<> 'style
          (<> "iframe {
    border: none;
    resize: both;
  overflow: auto;
}"))
        (<> '(iframe :name "report-frame"  
              :id "report-frame"
              :onload "this.width=$(\".container\").width();
                       var h = $(window).height() - $('#report-frame').offset().top - 5;
                         $('#report-frame').height(h);"
              :style " overflow: scroll;")
          " ")))


(defun transaction-bordereau-post (contract-numbers
                                          start-date
                                          interval)
   (assert (and contract-numbers start-date interval) nil
            "The data is wrong or incomplete

Numbers : ~A~%start:~A ~%interval: ~A"
            contract-numbers start-date interval)
   (maxclaims::with-adb 
        (let ((result 
               (transaction-bordereau contract-numbers start-date interval)))
          (if (string-equal result "NULL")
              "[]"
              result))))



(hunchentoot:define-easy-handler
    (ttb-report-handler
     :uri "/ecm/report/transaction-bordereau")
    (contract-numbers
     start-date
     interval
     json
     spreadsheet-type)
  (or (ignore-errors (ecm/user:call-with-user (constantly t)))
      (hunchentoot:redirect "/ecm/login"))

  (if (string-equal "POST" (hunchentoot:request-method*))
      (with-output-to-string (sexpml:*sexpml-output*)
        (let ((js-array (transaction-bordereau-post
                         contract-numbers
                         start-date
                         interval)))
          (if json 
              (<> :unescaped js-array)
              (if (string= js-array "[]")
                  (<> :text "Nothing to report")
                  (let* ((ss (ecm/report/transaction-bordereau:transaction-bordereau-spreadsheet js-array))
                         (file (ecm/spreadsheet:make-spreadsheet-file 
                                ss :type (if (string= spreadsheet-type "in-browser")
                                             "html"
                                             spreadsheet-type))))
                  
                  
                    (if (string= spreadsheet-type "in-browser")
                        (<> :unescaped
                          (alexandria:read-file-into-string file))
                        (progn 
                          (setf (hunchentoot:header-out 
                                 "Content-Disposition")
                                (format nil "attachment; filename=\"~A.~A\""
                                        (pathname-name file)
                                        (pathname-type file)))
                          (hunchentoot:handle-static-file file))))))))
      (<transaction-bordereau-page>)))
