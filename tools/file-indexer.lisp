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
               (sqlite:execute-non-query
                db
                "create table if not exists files (name text)")
               (sqlite:execute-non-query
                db
                "create table if not exists shasums (sum text)")
               (sqlite:execute-non-query
                db
                "create unique index if not exists files_unique_name on files(name)")
               (sqlite:execute-non-query
                db
                "create unique index if not exists shasums_unique_sum on shasums(sum)")
               (sqlite:execute-non-query
                db
                "create table if not exists
                  files_shasums (file integer,
                                 shasum integer,
                                 foreign key (file) references files(rowid),
                                 foreign key (shasum) references shasums(rowid))")
               (sqlite:execute-non-query
                db
                "create unique index if not exists shasums_files_unique_assuc on files_shasums(file,shasum)")
               (loop for file in files
                     do
                        (format t "Processing file ~s~%" file)
                        (sqlite:with-transaction db
                          (let* ((sum (ironclad:byte-array-to-hex-string
                                       (ironclad:digest-file :sha256 file)))
                                 (file-id
                                   (progn
                                     (sqlite:execute-single
                                      db "insert into files (name) values (?) on conflict do nothing" file)
                                     (sqlite:execute-single db "select rowid from files where name = ?" file)))
                                 (sum-id
                                   (progn
                                     (sqlite:execute-single
                                      db "insert into shasums (sum) values (?) on conflict do nothing" sum)
                                     (sqlite:execute-single db "select rowid from shasums where sum = ?" sum))))
                            (sqlite:execute-single db
                                                   "insert into files_shasums (file,shasum) values (?,?)
                                                    on conflict do nothing"
                                                   file-id
                                                   sum-id)
                            (format t "Done with file ~s, sum: ~s~%" file sum))))))
           (terpri)))))

(defun dump ()
  (setf net.didierverna.clon:*context* nil
        *features* (remove :fw.dump *features*))
  (net.didierverna.clon:dump "file-indexer" main))
