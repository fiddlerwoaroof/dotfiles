(defpackage :fwoar.json-formatter
  (:use :cl )
  (:export :main :prepare-dump :dump))
(in-package :fwoar.json-formatter)

(defgeneric pprint-json (client obj context stream))

(defmethod pprint-json ((client (eql :flat)) (obj string) context stream)
  (format stream "\"~a\"" (serapeum:string-replace-all "\"" obj "\\\"")))
(defmethod pprint-json ((client (eql :flat)) (obj float) context stream)
  (format stream "~a" obj))
(defmethod pprint-json ((client (eql :flat)) (obj integer) context stream)
  (format stream "~a" obj))

(defmethod pprint-json ((client (eql :flat)) (obj hash-table) context stream)
  (let* ((keys (sort (alexandria:hash-table-keys obj)
                     'string-lessp))
         (id (find "id" keys :test 'equal))
         (keys (remove "id" keys :test 'equal)))
    (princ "{" stream)
    (if id
        (pprint-json client id context stream)
        (pprint-json client (car keys) context stream))
    (princ ": " stream)
    (if id
        (pprint-json client (gethash id obj) obj stream)
        (pprint-json client (gethash (car keys) obj) obj stream))
    (mapc (lambda (key)
            (princ ", " stream)
            (pprint-json client key context stream)
            (princ ": " stream)
            (pprint-json client (gethash key obj) obj stream))
          (if id
              keys
              (cdr keys)))
    (princ "}" stream)))

(defmethod pprint-json ((client (eql :flat)) (obj array) context stream)
  (pprint-logical-block (stream nil :prefix "[" :suffix "]")
    (loop for sub across obj
          for first = t then nil
          unless first
            do (princ "," stream) (pprint-newline :mandatory stream)
          do (pprint-json client sub obj stream))))

(defvar *synopsis*
  (net.didierverna.clon:defsynopsis (:postfix "PATHS" :make-default nil)
    (flag :short-name "h" :long-name "help")))

(defun main ()
  (let* ((context (net.didierverna.clon:make-context :synopsis *synopsis*))
         (net.didierverna.clon:*context* context))
    (cond ((net.didierverna.clon:getopt :context context
                                        :long-name "help")
           (net.didierverna.clon:help))


          (t
           (let ((input (first (net.didierverna.clon:remainder :context context))))
             (with-open-file (s input)
               (let ((decoded (com.inuoe.jzon:parse s)))
                 (fresh-line)
                 (pprint-json :flat decoded nil *standard-output*))))
           (fresh-line)))))

(defun prepare-dump ()
  (setf net.didierverna.clon:*context* nil
        *features* (remove :fw.main (remove :fw.dump *features*))))

(defun dump ()
  (prepare-dump)
  (net.didierverna.clon:dump "json-formatter" main))
