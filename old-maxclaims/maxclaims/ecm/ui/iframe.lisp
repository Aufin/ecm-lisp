(defpackage :ecm/ui/iframe
  (:use :cl)
  (:import-from :ecm/ml)
  (:import-from :ecm/ps)
  (:export #:<iframe-load-resize>))
(in-package :ecm/ui/iframe)

(defun <iframe-load-resize> (pound-iframe-id &key (to-element "body"))
  (ecm/ml:<> 'html5:script
    "

function resizeIFrameToFitContent( iFrame ) {

    iFrame.width  = iFrame.contentWindow.document.body.scrollWidth;
    iFrame.height = iFrame.contentWindow.document.body.scrollHeight + 5;
};

$('"pound-iframe-id"').on('load', function() { resizeIFrameToFitContent(this);});
"))
