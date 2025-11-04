(defpackage :fwoar.embedding-hash
  (:use :cl )
  (:export main))
(in-package :fwoar.embedding-hash)

(defun embed-request (content)
  (let ((drakma:*text-content-types* '(("application" . "json"))))
    (yason:parse
     (drakma:http-request "http://localhost:11434/api/embed"
                          :method :post
                          :content-type "application/json"
                          :external-format-out :utf-8
                          :external-format-in :utf-8
                          :content content)
     :json-arrays-as-vectors t)))

(defun embed-ollama (title &optional (model "nomic-embed-text"))
  (let ((yason:*symbol-key-encoder* 'yason:encode-symbol-as-lowercase))
    (gethash "embeddings"
             (embed-request
              (yason:with-output-to-string* ()
                (yason:encode-alist
                 `((:model . ,model)
                   (:stream . yason:false)
                   (:input . ,title))))))))

(defun dot-product (a b)
  (assert (= (length a) (length b)))
  (loop for x across a
        for y across b
        sum (* x y)))

(defun magnitude (vec)
  (sqrt (loop for item across vec
              sum (expt item 2))))

(defun cosine-similarity (a b)
  (/ (dot-product a b)
     (* 1.0d0
        (magnitude a)
        (magnitude b))))

(defvar *synopsis*
  (net.didierverna.clon:defsynopsis (:postfix "FILE" :make-default nil)
    (flag :short-name "h" :long-name "help")))

(defun main ()
  (let* ((context (net.didierverna.clon:make-context :synopsis *synopsis*))
         (net.didierverna.clon:*context* context))
    (cond ((net.didierverna.clon:getopt :context context
                                        :long-name "help")
           (net.didierverna.clon:help))
          (t
           (format t
                   "~&~a~%"
                   (string-downcase
                    (bit-smasher:bits->hex
                     (map '(vector bit)
                          (lambda (it)
                            (if (>= it 0) 1 0))
                          (elt (embed-ollama
                                (format nil "clustering: ~a"
                                        (alexandria:read-file-into-string
                                         (car (net.didierverna.clon:remainder :context context)))))
                               0)))))))))

(defun prepare-dump ()
  (setf net.didierverna.clon:*context* nil
        *features* (remove :fw.main (remove :fw.dump *features*))))

#+(or)
(defun dump ()
  (setf net.didierverna.clon:*context* nil
        *features* (remove :fw.dump *features*))
  (net.didierverna.clon:dump "file-indexer" main))
