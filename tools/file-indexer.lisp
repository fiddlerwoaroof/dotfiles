#+fw.dump
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp")
  (require :asdf))

#+fw.dump
(progn
  (asdf:load-asd
   (asdf:system-relative-pathname :net.didierverna.clon "termio/net.didierverna.clon.termio.asd"))
  (require :uiop))

#+fw.dump
(ql:quickload '(:net.didierverna.clon :ironclad :sqlite))

(defpackage :fwoar.file-indexer
  (:use :cl)
  (:export ))
(in-package :fwoar.file-indexer)
(defvar *synopsis*
  (net.didierverna.clon:defsynopsis (:postfix "DB FILES..." :make-default nil)
    (flag :short-name "h" :long-name "help")))


(defun main ()
  (let* ((context (net.didierverna.clon:make-context :synopsis *synopsis*))
         (net.didierverna.clon:*context* context))
    (cond ((net.didierverna.clon:getopt :context context
                                        :long-name "help")
           (net.didierverna.clon:help))
          (t
           (destructuring-bind (db-fn . files)
               (net.didierverna.clon:remainder :context context)
             (sqlite:with-open-database (db db-fn)
               (sqlite:execute-non-query db "pragma journal_mode=wal")
               (sqlite:execute-non-query
                db
                "create table if not exists
                  files_shasums (file text,
                                 shasum text,
                                 size bigint)")
               (sqlite:execute-non-query
                db
                "create unique index if not exists shasums_files_unique_assuc on files_shasums(file,shasum)")
               (loop for raw-file in files
                     for file = (uiop:parse-native-namestring raw-file)
                     do
                        (sqlite:with-transaction db
                          (with-open-file (s file :element-type '(unsigned-byte 8))
                            (let* ((sum (ironclad:byte-array-to-hex-string
                                         (ironclad:digest-file :sha256 file)))
                                   (length (file-length s)))
                              (sqlite:execute-single db
                                                     "insert into files_shasums (file,shasum,size)
                                                      values (?,?,?)
                                                      on conflict do nothing"
                                                     (uiop:native-namestring (truename file))
                                                     sum
                                                     length)))))))
           (terpri)))))

(defun dump ()
  (setf net.didierverna.clon:*context* nil
        *features* (remove :fw.dump *features*))
  (net.didierverna.clon:dump "file-indexer" main))
