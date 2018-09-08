(defpackage :gap-dates
  (:use :cl))
(in-package :gap-dates)

(defun gap-sequence (seq diff-fun diff)
  (reverse (reduce (lambda (accum next)
                     (let ((prev (car accum)))
                       (if (>= (funcall diff-fun next prev) diff)
                           (cons next accum)
                           accum)))
                   (cdr seq)
                   :initial-value (list (elt seq 0)))))

(defun read-dates (dates)
  (mapcar (lambda (date)
            (local-time:parse-timestring date))
          dates))

(defun prune-dates (dates exclude-last-n)
  (let ((last-date-idx (- (length dates) 
                          exclude-last-n)))
    (append (gap-sequence (subseq dates 0 last-date-idx)
                          'local-time:timestamp-difference
                          #.(* 60 60 24 7))
            (subseq dates last-date-idx))))

(defun get-dates ()
  (mapcar (alexandria:compose 'car
                              'last
                              'pathname-directory)
          (uiop:directory* "~/.feed-archive/????-??-??")))

(defun main (inp)
  (let ((local-time:*default-timezone* local-time:+utc-zone+))
    (let* ((dates (read-dates inp))
           (keep (prune-dates dates 31)))
      (mapcar (serapeum:op
                (local-time:format-timestring nil _ :format local-time:+iso-8601-date-format+))
              (set-difference dates keep :test 'equal)))))
