(in-package :maxclaims)


;; Portions of this file from trivial-configuration-parser :
;; Copyright (c) 2005 Brian Mastenbrook

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defvar *configuration-file-name* "maxclaims.conf")
(defvar *configuration-file-location* #P"/etc/maxclaims/")

(defun find-configuration-file ()
  "The rules are simple : 
0.5) look for it in ~/bin/, and also my directory for it.
1) if there is a file named *configuration-file-name* in *default-pathname-defaults*, we use it 
2) otherwise look for it in *configuration-file-location* 
3) lastly, try to find it in the source directory.

TODO: it would be nice to pass this at the command line one day."
  (error "Old conf unused. don't call find!")
  (let ((possible-locations (list (merge-pathnames "bin/" (user-homedir-pathname))
				  #P"/home/drewc/maxclaims/"
				  *default-pathname-defaults*
				  *configuration-file-location* 
				  (or (ignore-errors
					(asdf:component-pathname (asdf:find-system :maxclaims)))
				      ""))))
    (flet ((%find-configuration-file (&rest args)
	     (warn "Searching for config file ~A ~A" *configuration-file-name* args)
	     (apply #'merge-pathnames *configuration-file-name* args)))
      (some #'probe-file (mapcar #'%find-configuration-file possible-locations)))))


(defun load-configuration-file ()
  (let* ((find (find-configuration-file))
         (conf (parse-configuration-file (prog1 find (when find (warn "Using ~A" find)))))
	 (db-conf (configuration-value conf :database))
	 (backend-conf (configuration-value conf :backend)))
;;    (break "~A : ~A" db-conf (ignore-errors (configuration-value db-conf :port)))
    (setf *db-admin-parameters* 
	  (list (configuration-value db-conf :database)
		(configuration-value db-conf :user)
		(configuration-value db-conf :password)
		(configuration-value db-conf :host)
                :port (or (ignore-errors (configuration-value db-conf :port)) 5432)))
    (setf *db-connection-parameters* *db-admin-parameters*)
    (setf *backend-parameters* 
	  (cons (configuration-value backend-conf :backend) 
		(mapcan (lambda (item)
			  (unless (eq :backend (car item))
			    (list (car item) (cdr item))))
			(cdr backend-conf))))
    (setf *attachment-directory* (configuration-value conf :attachment-directory))
    (setf *attachment-trash-directory* (configuration-value conf :attachment-trash-directory))
    (setf *csv-report-directory* (configuration-value conf :csv-output-directory))))

(defun parse-configuration-file (pathname)
  (let ((*readtable* (copy-readtable *readtable*))
        (*package* (find-package :keyword))
        (*read-eval* nil)
        (*read-base* 10)
        (*read-default-float-format* 'double-float)
        (*read-suppress* nil))
    (setf (readtable-case *readtable*) :upcase)
    (set-syntax-from-char #\# #\;)
    (set-syntax-from-char #\; #\ )
    (set-macro-character #\{
                         (lambda (stream char)
                           (declare (ignore char))
                           (read-delimited-list #\} stream t)))
    (set-macro-character #\}
                         (lambda (stream char)
                           (declare (ignore stream char))
                           (error "Unmatched close brace!")))
    (set-syntax-from-char #\: #\ )
    (with-open-file (file pathname :direction :input)
      (let* ((eof-value (gensym "EOF"))
             (read-file (loop for thing = (read file nil eof-value)
			   while (not (eql thing eof-value))
			   collect thing)))
        (labels ((parse-file (read-file)
                   (loop
                      with waiting-identifier = nil
                      with mode = nil
                      for value in read-file
                      if waiting-identifier
                      if (and (listp value) (not mode))
                      collect (cons waiting-identifier
                                    (cons 'module-marker
                                          (parse-file value)))
                      else if (eq value :=)
                      do (setf mode :single-value)
                      else if (eq mode :single-value)
                      collect (cons waiting-identifier value)
                      else do (error "Unexpected input: ~A" value)
                      if (and waiting-identifier
                              (not (eq value :=)))
                      do (setf waiting-identifier nil
                               mode nil)
                      else if (and (keywordp value)
                                   (not (eq value :=)))
                      do (setf waiting-identifier value)
                      else unless (eq value :=)
                      do (error "Unexpected input: ~A" value))))
          (cons (merge-pathnames pathname)
                (parse-file read-file)))))))

(defvar *current-module-path* nil)

(defun configuration-values (parsed-configuration-file key &rest inner-modules)
  (labels
      ((%configuration-values (search-list key inner-modules)
         (if inner-modules
             (let* ((to-find (car inner-modules))
                    (found-list (member to-find search-list
                                        :key #'car)))
               (if found-list
                   (if (not (and (consp (cdr (first found-list)))
                                 (eq (car (cdr (first found-list)))
                                     'module-marker)))
                       (error "The value ~A was found, but it wasn't a module in the configuration file ~A"
                              (string-downcase (symbol-name to-find))
                              (namestring (car parsed-configuration-file)))
                       (nconc (%configuration-values (cddr (first found-list))
                                                     key
                                                     (cdr inner-modules))
                              (%configuration-values (cdr found-list)
                                                     key inner-modules)))))
             (let ((found-list (member key search-list
                                       :key #'car)))
               (if found-list
                   (cons (cdr (first found-list))
                         (%configuration-values (cdr found-list) key nil)))))))
    (%configuration-values (cdr parsed-configuration-file) key
                           (reverse inner-modules))))

(defun configuration-value (parsed-configuration-file key &rest inner-modules)
  (let ((file-path (car parsed-configuration-file)))
    (labels
        ((%configuration-value (search-list key inner-modules)
           (if inner-modules
               (let* ((to-find (car inner-modules))
                      (found-list (member to-find search-list :key #'car))
                      (*current-module-path* (cons to-find *current-module-path*))
                      (others (find to-find (cdr found-list) :key #'car)))
                 (if found-list
                     (if (not others)
                         (if (not (and (consp (cdr (first found-list)))
                                       (eq (car (cdr (first found-list)))
                                           'module-marker)))
                             (error "The value ~A was found, but it wasn't a module in the configuration file ~A"
                                    (string-downcase (symbol-name to-find))
                                    (namestring (car parsed-configuration-file)))
                             (%configuration-value (cddr (first found-list))
                                                   key (cdr inner-modules)))
                         (error "There is a duplicate module or value
                         named ~A in the path ~{~A~^/~} in the
                         configuration file ~A."
                                (string-downcase (symbol-name to-find))
                                (reverse
                                 (mapcar #'(lambda (to-find)
                                             (string-downcase (symbol-name to-find)))
                                         (cdr *current-module-path*)))
                                (namestring file-path)))
                     (error "The module ~{~A~^/~} was not found in the configuration file ~A"
                            (reverse
                             (mapcar #'(lambda (to-find)
                                         (string-downcase (symbol-name to-find)))
                                     *current-module-path*))
                            (namestring file-path))))
               (let* ((found (member key search-list :key #'car))
                      (others (find key (cdr found) :key #'car)))
                 (if found
                     (if (not others)
                         (cdr (first found))
                         (error "There is a duplicate key named ~A in the path ~{~A~^/~} in the configuration file ~A."
                                (string-downcase (symbol-name key))
                                (reverse
                                 (mapcar #'(lambda (to-find)
                                             (string-downcase (symbol-name to-find)))
                                         (cdr *current-module-path*)))
                                (namestring file-path)))
                     (error "The key ~A was not found in the configuration file ~A in the path ~{~A~^/~}"
                            (string-downcase (symbol-name key))
                            (or (ignore-errors (namestring file-path))
                                file-path)
                            (reverse
                             (mapcar #'(lambda (to-find)
                                         (string-downcase (symbol-name to-find)))
                                     (cdr *current-module-path*)))))))))
      (%configuration-value (cdr parsed-configuration-file) key
                            (reverse inner-modules)))))

(defun write-configuration-file (parsed-configuration-file &rest arguments-to-open)
  (let* ((path (car parsed-configuration-file))
         (contents (cdr parsed-configuration-file))
         (open-file (apply #'open path :direction :output arguments-to-open)))
    (unwind-protect
         (let ((tab-level 0))
           (declare (special tab-level))
           (labels ((%write-configuration-file (contents)
                      (loop for module-or-value in contents
                         do (if (and (consp (cdr module-or-value))
                                     (eq (car (cdr module-or-value)) 'module-marker))
				(progn
				  (format open-file "~&~{~C~}~A {~%"
					  (loop for i below tab-level collect #\space)
					  (string-downcase (symbol-name (car module-or-value))))
				  (let ((tab-level (+ tab-level 4)))
				    (declare (special tab-level))
				    (%write-configuration-file (cddr module-or-value)))
				  (format open-file "~&~{~C~}}"
					  (loop for i below tab-level collect #\space)))
				(format open-file "~&~{~C~}~A = ~S;"
					(loop for i below tab-level collect #\space)
					(string-downcase (symbol-name (car module-or-value)))
					(cdr module-or-value))))))
             (%write-configuration-file contents)))
      (close open-file))))
