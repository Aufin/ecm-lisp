(in-package #:maxclaims)

;;; Not sure about the name or the full protocol yet. Once that is
;;; hashed out this should probably go into ucw-standard, or be
;;; replaced with a mixin description class?

;;; I'm not really happy with the interface, but it is no worse than
;;; paged-list and nothing better comes to mind that wouldn't require
;;; more effort than is worth spending on unifying ajax+non-ajax paged
;;; lists

;;; Major changes from paged-list
;;; - item number is not stored in the split pages
;;; - data is stored as a vector in the paged-set and is accessible
;;; - pages are displaced 1d arrays of length page-size offset by the
;;;   appropriate element count

;;; Portions taken from ucw

;;;; ** Range View

(defclass paged-set ()
  ((data :initarg :data :accessor paged-set.data)
   (offset :initarg :offset
           :accessor paged-set.offset
           :initform 0
           :backtrack t
           :documentation "Which of the pages we're currently looking at.")
   ;; it might be worth making pages a vector since we know before
   ;; partitioning how many pages there will be
   (pages :reader paged-set.pages :initform '())
   (page-size :accessor paged-set.page-size :initform 20 :initarg :page-size))
  (:documentation
   "Component for showing the user a set of data one \"page\" at a time.

The data set is presented one \"page\" at a time with links to
the the first, previous, next and last page. Each page shows
at most PAGE-SIZE elements of the data. The data is passed to
the paged-set at instance creation time via the :DATA initarg.

The generic function RENDER-PAGED-SET-ITEM is used to render
each item of DATA.

In order to change the rendering of the single elements of a
range view developer's should create a sub class of PAGED-SET
and define their RENDER-PAGED-SET-ITEM methods on that.")
  (:metaclass standard-component-class))

;;; Protocol
(defgeneric result-set.get-page (result-set-viewer &optional page))

(defun partition-set-into-pages (data page-size)
  (let* ((data-length (length data))
	 (page-count (floor (/ data-length page-size))))
    (loop with pages = '()
       for offset from 0 to (1- page-count)
       ;; do + push instead of collect to make pushing the final page easier
       do (push (make-array (list page-size)
			    :displaced-to data
			    :displaced-index-offset (* offset page-size))
		pages)
       finally (if (> data-length (* page-count page-size))
		   (push (make-array (list (- data-length (* page-count page-size)))
				     :displaced-to data
				     :displaced-index-offset (* page-count page-size))
			 pages))
       finally (return (nreverse pages)))))

(defmethod shared-initialize :after ((range paged-set) slot-names
                                     &key data &allow-other-keys)
  (declare (ignore slot-names))
  (setf (paged-set.data range) data))

(defmethod (setf paged-set.data) :around (new-data (paged-set paged-set))
  (let ((vector-data (coerce new-data 'vector)))
    (prog1 (call-next-method vector-data paged-set)
      (setf (slot-value paged-set 'pages)
	    (partition-set-into-pages vector-data (paged-set.page-size paged-set))))))

(defmethod (setf paged-set.page-size) :before (new-size (paged-set paged-set))
  (when (not (= (slot-value paged-set 'page-size) new-size))
    (setf (slot-value paged-set 'pages)
	  (partition-set-into-pages (paged-set.data paged-set) new-size))))

(defmethod paged-set.emptyp ((set paged-set))
  (= 0 (length (paged-set.data set))))

(defmethod paged-set.get-page ((set paged-set) &optional (page (paged-set.current-page set)))
  (nth page (paged-set.pages set)))

(defmethod paged-set.current-page ((range paged-set))
  (paged-set.get-page range (paged-set.offset range)))

(defmethod paged-set.have-previous-p ((view paged-set))
  "Returns true if VIEW has a page before the current one."
  (and (paged-set.pages view)
       (not (zerop (paged-set.offset view)))))

(defmethod paged-set.have-next-p ((view paged-set))
  "Returns true if VIEW has a page after the current one."
  (with-slots (offset pages)
      view
    (and pages (< offset (1- (length pages))))))

(defmethod paged-set.page-count ((list paged-set))
  (length (paged-set.pages list)))

(defgeneric render-paged-set-item (paged-set item)
  (:documentation "Render a single element of a paged-set.")
  (:method ((paged-set paged-set) (item t))
    "Standard implementation of RENDER-PAGED-SET-ITEM. Simply
applies ITEM to princ (via <:as-html)."
    (declare (ignore paged-set))
    (<:as-html item)))

(defaction scroll-start ((range paged-set))
  (setf (paged-set.offset range) 0))

(defaction scroll-end ((range paged-set))
  (setf (paged-set.offset range) (1- (length (paged-set.pages range)))))
  
(defaction scroll-forward ((view paged-set) &optional (n 1))
  (with-slots (offset pages)
      view
    (incf offset n)
    (when (<= (length pages) offset)
      (scroll-end view))))

(defaction scroll-backward ((range paged-set) &optional (n 1))
  (with-slots (offset)
      range
    (decf offset n)
    (when (minusp offset)
      (setf offset 0))))

(defaction scroll-to-page ((range paged-set) page-number)
  (setf (paged-set.offset range) page-number))

;;; function to split :data into displaced arrays with a fill-pointer of page-size

;;; :around for (setf result-set) to coerce to vector

;;; :around for result-set.pages to initialize if unbound

;;; :before (setf page-size) to recompute pages if new page-size is different

;;; copy paged-list actions


;; Copyright (c) 2003-2005 Edward Marco Baringer
;; Copyright (c) 2011 ... FIXME: tech.coop?
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
