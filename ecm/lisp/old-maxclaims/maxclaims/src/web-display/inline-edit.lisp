(defpackage :maxclaims/web-display/inline-edit
  (:use :cl)
  (:export #:<script-inline-edit>))
  

(in-package :maxclaims/web-display/inline-edit)

(defun <script-inline-edit> ()
  (<:script 
   (<:as-is
    (symbol-name '#:|

var inline_edit ;
var inline_edit_button;

function maxInlineEditModal (url) {
   $('#myModal').load(url);
   $('#myModal').modal('show').css({
    'margin-top': function () { //vertical centering
        return -($(this).height() / 2);
    },
    'margin-left': function () { //Horizontal centering
        return -($(this).width() / 2);
    }
});
};

function maxInlineEdit(editable) {
    // first, find any existing #inline-edit and remove them.
    var previous =  $('#inline-edit')[0];
    // and remove it
    $( previous).remove();

    var json = editable.attr('data-inline-edit');
    var jso = JSON.parse(json)
    var url = '/ecm/inline-edit?json=' + encodeURIComponent(JSON.stringify(jso));
    var on = "maxInlineEditModal('" + url +"')"

   $('[data-toggle="tooltip"]').tooltip('destroy');
 //    console.log(on);
    var a = $('<a style="float:right" data-toggle="tooltip" class="inline-edit btn" id="inline-edit" title="Click to Edit this." />');
    var span = $('<span class="icon icon-th-list"/>');
    a.append(span);
    editable.append(a);
    inline_edit = editable;
    inline_edit_button = a;
    $( a ).hover(undefined, function() {$( this ).remove()})
    $('#inline-edit').click(function () {maxInlineEditModal(url)});
    $('#inline-edit').hover(undefined, function() {$( this ).remove()});
//    $('[data-toggle="tooltip"]').tooltip({position : 'left top'});
  return 1 ; 
};

$('[data-inline-edit]')
   .hover(function () {
            if (this !== inline_edit) {
              maxInlineEdit($( this ));
              $('[data-toggle="tooltip"]').tooltip({placement: 'bottom'});
            }
          }, 
          function () {$('[data-toggle="tooltip"]').tooltip('destroy');
                       $('.tooltip').remove();                       

                       });
|))))


  
