(eval-when (:compile-toplevel :load-toplevel)
  (labels ((ensure-library (lib)
             (unless (asdf:component-loaded-p lib)
               (ql:quickload lib)))
           (ensure-libraries (&rest libs)
             (mapcar #'ensure-library libs)))
    (ensure-libraries :sqlite
                      :data-lens
                      :ironclad
                      :fwoar-lisputils)))

(defpackage :fwoar.tag-manager
  (:use :cl )
  (:export ))
(in-package :fwoar.tag-manager)

(defvar *db-location*
  (merge-pathnames "tag-db.sqlite"
                   (user-homedir-pathname)))

(defvar *store-location*
  (merge-pathnames (make-pathname :directory (list :relative ".tag-store"))
                   (user-homedir-pathname)))

(defvar *db*)

(defmacro doto (it &body body)
  (let ((it-sym (gensym "IT")))
    `(let ((,it-sym ,it))
       (prog1 ,it-sym
         ,@(mapcar (lambda (it)
                     `(,(car it)
                       ,it-sym
                       ,@(cdr it)))
                   body)))))

(defun start (&optional (*db-location* *db-location*))
  (doto (setf *db* (sqlite:connect *db-location*))
    (initialize-schema)))

(defun initialize-schema (db)
  (sqlite:execute-non-query
   db
   "PRAGMA journal_mode=WAL")
  (sqlite:execute-non-query
   db
   "create table if not exists tags (
      id text default (uuid()) primary key,
      name text unique)")
  (sqlite:execute-non-query
   db
   "create table if not exists tag_relations (
      tag_a text references tag(id),
      tag_b text references tag(id),
      primary key (tag_a, tag_b))")
  (sqlite:execute-non-query
   db
   "create table if not exists tag_files (
      tag text references tag(id),
      file_hash text,
      primary key (tag, file_hash))"))

(defun split-hash (hash)
  (let ((parts  (loop for x from 0 below 64 by 16
                      for y = (+ x 16)
                      collect (ironclad:byte-array-to-hex-string
                               (coerce (subseq hash x y)
                                       '(vector (unsigned-byte 8)))))))
    (make-pathname :directory (list* :relative (butlast parts))
                   :name (car (last parts)))))

(defun hash-for-stream (s)
  (ironclad:byte-array-to-hex-string (ironclad:digest-stream :sha256 s)))

(defun tag-sql (db hash tag)
  (sqlite:execute-non-query db
                            "insert or ignore into tags (name) values (?)"
                            tag)
  (let ((tag-id (sqlite:execute-one-row-m-v db
                                            "select id from tags where name = ?"
                                            tag)))
    (sqlite:execute-non-query db
                              "insert or ignore into tag_files (tag, file_hash) values (?,?)"
                              tag-id
                              hash)))

#+(or)
(hash->path "e45b72f5c0c0b572db4d8d3ab7e97f368ff74e62347a824decb67a84e5224d75")

(defun hash->path (hash)
  (make-pathname :directory (list :relative
                                  (subseq hash 0 2)
                                  (subseq hash 0 4)
                                  (subseq hash 0 8))
                 :name hash
                 :type "ref"))

(defun symlink (oldpath newpath)
  #+sbcl
  (restart-case (sb-posix:symlink oldpath newpath)
    (continue () :report (lambda (s)
                           (declare (type stream s)
                                    (optimize (speed 1) (space 3)))
                           (format s "don't symlink ~s -> ~s" oldpath newpath))
      nil)
    (force () :report (lambda (s)
                        (format s "force symlinking ~s -> ~s" oldpath newpath))
      (sb-posix:unlink newpath)
      (symlink oldpath newpath))))

(defun tag-file (db path tag)
  (let ((hash (fw.lu:closing (hash-for-stream (open path :element-type '(unsigned-byte 8))))))
    (tag-sql db hash tag)
    (symlink path
             (namestring
              (ensure-directories-exist
               (merge-pathnames (hash->path hash)
                                *store-location*))))))
