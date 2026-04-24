(defpackage :fwoar.explode-lab
  (:use :cl )
  (:export ))
(in-package :fwoar.explode-lab)

(defun read-pnm (s)
  (assert (equal "P3" (serapeum:trim-whitespace (read-line s))))
  (destructuring-bind (w h) (mapcar #'parse-integer (serapeum:tokens (read-line s)))
    (let* ((max-color (mapcar #'parse-integer (serapeum:tokens (read-line s))))
           (out (make-array (list w h 3) :element-type `(integer 0 ,max-color))))
      (:printv max-color w h)
      (loop for x from 0 below h
            for line = (serapeum:trim-whitespace (read-line s))
            do (loop for token in (serapeum:tokens line)
                     for val = (parse-integer (serapeum:trim-whitespace token))
                     for y from 0
                     do (setf (aref out x (floor y 3) (mod y 3)) val)))
      out)))

(defun write-pnm (s img-data max writer)
  (let ((dims (array-dimensions img-data))
        (max-width (floor (log max 10))))
    (destructuring-bind (w h . _) dims
      (declare (ignore _))
      (prog1 s
        (format s "P3~%~d ~d~%~d~%" w h max)
        (funcall writer img-data max max-width w h s)

        (fresh-line s)))))

(defun write-identity (img-data max max-width w h s)
  (declare (ignore max))
  (loop for x below w
        do (loop for y below h
                 unless (= y 0)
                   do (format s " ")
                 do (format s "~vd ~vd ~vd"
                            max-width (aref img-data x y 0)
                            max-width (aref img-data x y 1)
                            max-width (aref img-data x y 2)))
        do (terpri s)))

(defun translate-coord (img x y)
  (multiple-value-bind (l a b)
      (multiple-value-call #'dufy:xyz-to-lab
        (dufy:qrgb-to-xyz (aref img x y 0)
                          (aref img x y 1)
                          (aref img x y 2)))
    (list l a b)))


(defun write-norm-b (new-b)
  (lambda (img-data max max-width w h s)
    (declare (ignore max max-width))
    (loop for x below w
          do (loop for y below h
                   for (l a b) = (translate-coord img-data x y)
                   unless (= y 0)
                     do (format s " ")
                   do (multiple-value-call #'format t "~3a ~3a ~3a"
                        (multiple-value-call #'dufy:xyz-to-qrgb (dufy:lab-to-xyz l a new-b))))
          do (terpri s))))

(defun write-norm-a (new-a)
  (lambda (img-data max max-width w h s)
    (declare (ignore max max-width))
    (loop for x below w
          do (loop for y below h
                   for (l a b) = (translate-coord img-data x y)
                   unless (= y 0)
                     do (format s " ")
                   do (multiple-value-call #'format t "~3a ~3a ~3a"
                        (multiple-value-call #'dufy:xyz-to-qrgb (dufy:lab-to-xyz l new-a b))))
          do (terpri s))))

(defun write-rand-ab ()
  (lambda (img-data max max-width w h s)
    (declare (ignore max max-width))
    (loop for x below w
          do (loop for y below h
                   for (l a b) = (translate-coord img-data x y)
                   unless (= y 0)
                     do (format s " ")
                   do (multiple-value-call #'format t "~3a ~3a ~3a"
                        (multiple-value-call #'dufy:xyz-to-qrgb
                          (dufy:lab-to-xyz l
                                           (- 128d0 (random 255d0))
                                           (- 128d0 (random 255d0))))))
          do (terpri s))))


#+(or)
(

 (loop for x below 540
       do (loop for y below 540
                for (l a b) = (translate-coord *img* x y)
                do (multiple-value-call #'format t "~3a ~3a ~3a "
                     (multiple-value-call #'dufy:xyz-to-qrgb (dufy:lab-to-xyz l a 7.998833117526949d0))))
          (terpri))

 )
