;; Some snippets not currently used but possibly useful

(defun fwoar:doc-for-resource-type (resource-type)
  (cl-format nil
             "https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/~{~a~^-~}"
             (list* "aws" "resource"
                    (cdr (s-split "::" (s-downcase resource-type))))))

(defun fwoar:document-cf (min max)
  "Jump to AWS Cloudformation docs for a resource type.

MIN: the point in the buffer where the resource type starts
MAX: the point in the buffer where the resource type ends

This uses (interactive \"r\") to automagically populate the arguments
from the selected region."
  (interactive "r")
  (browse-url
   (fwoar:doc-for-resource-type
    (buffer-substring min max))))
