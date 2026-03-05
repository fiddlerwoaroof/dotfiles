(defpackage :git-pick-patch
  (:use :cl :alexandria :serapeum)
  (:export :main))
(in-package :git-pick-patch)

(defun read-header (inp)
  (string-join (loop for line = (read-line inp nil)
                  while line
                  collect line
                  until (eql #\@ (peek-char nil inp nil)))
               #\newline))

(defun read-hunk (inp)
  (when (eql #\@
             (peek-char nil inp nil))
    (string-join (loop for line = (read-line inp nil)
                    while line
                    collect line
                    until (member (peek-char nil inp nil) '(#\@ #\d)))
                 #\newline)))

(defun read-hunks (inp)
  (loop for hunk = (read-hunk inp)
     while hunk
     collect hunk))

(defun get-file-patch (inp)
  (list (read-header inp)
        (read-hunks inp)))

(defun get-all-patches (inp)
  (loop for patch = (get-file-patch inp)
     for (header data) = patch
     while (and (string/= header "")
                (not (null data)))
     collect patch))

(defun filter-hunks (hunks predicate)
  (remove-if-not predicate hunks))

(defun filter-file-hunks (file-data predicate)
  (let ((results (filter-hunks (cadr file-data)
                               predicate)))
    (when results
      (list (car file-data)
            results))))

(defun filter-patch (patch-data predicate)
  (remove-if #'null (mapcar (lambda (x)
                              (filter-file-hunks x predicate))
                            patch-data)))

(defun combine-hunks (hunks)
  (string-join hunks #\newline))

(defun rebuild-file-patch (file-data)
  (destructuring-bind (header hunks) file-data
    (format nil "~a~%~a" header (combine-hunks hunks))))

(defun rebuild-patch (patch-data)
  (string-join (mapcar #'rebuild-file-patch patch-data)
               #\newline))

(defvar *synopsis*
  (net.didierverna.clon:defsynopsis (:postfix "RULE ..." :make-default nil)
    (flag :short-name "h" :long-name "help")
    (enum :short-name "m" :long-name "mode" :description "mode for interpreting rules"
          :default-value '|regex|
          :enum '(|number| |regex| |substring|))))

(defun filter-patch (predicate s)
  (serapeum:with-collector (collect-patch)
    (loop for patch = (get-file-patch s)
          for filtered = (filter-file-hunks patch predicate)
          until (equal patch '("" nil))
          when filtered do
            (format t "~&~a~&" (rebuild-file-patch filtered)))))

(defun main ()
  (let* ((context (net.didierverna.clon:make-context :synopsis *synopsis*))
         (net.didierverna.clon:*context* context)
         (patterns (net.didierverna.clon:remainder :context context))
         (mode (net.didierverna.clon:getopt :context context
                                            :long-name "mode")))
    (cond ((or (net.didierverna.clon:getopt :context context
                                            :long-name "help")
               (null patterns)
               (cdr patterns))
           (net.didierverna.clon:help))
          (t
           (let ((pattern (car patterns)))
             (loop for patch = (get-file-patch *standard-input*)
                   for filtered = (when patch
                                    (case mode
                                      (|number|
                                       (error "not implemented"))
                                      (|substring|
                                       (error "not implemented"))
                                      (|regex|
                                       (filter-file-hunks patch
                                                          (op (cl-ppcre:scan pattern _))))))
                   until (equal patch '("" nil))
                   when filtered do
                     (format t "~&~a~&" (rebuild-file-patch filtered))))))))


#+(or fw.dump fw.main)
(defun prepare-dump ()
  (setf net.didierverna.clon:*context* nil
        *features* (remove :fw.main (remove :fw.dump *features*))
        *print-case* :downcase))
#+(or fw.dump fw.main)
(defun dump (&optional out-path)
  (prepare-dump)
  (net.didierverna.clon:dump (if out-path
                                 (format nil "~a/zenburn" out-path)
                                 "zenburn")
                             main))
