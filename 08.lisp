(defun node-child-count (node) (first node))
(defun node-metadata-count (node) (second node))
(defun node-children (node) (third node))
(defun node-metadata (node) (fourth node))

(defun traverse-tree (root &key (callback #'identity))
  (when root (funcall callback root))
  (loop for child in (node-children root)
     do (traverse-tree child :callback callback)))


(defun parse-node (stream)
  (let* ((child-count (read stream))
         (metadata-count (read stream))
         (children (loop for _ from 0 below child-count collect (parse-node stream)))
         (metadata (loop for _ from 0 below metadata-count collect (read stream))))
    (list child-count metadata-count children metadata)))

(defun sum-metadata (root)
  (let ((sum 0))
    (flet ((summer (node)
             (incf sum (apply #'+ (node-metadata node)))))
      (traverse-tree root :callback #'summer))
    sum))

(defparameter *cache* (make-hash-table :test 'equalp))
(defun node-value (node)
  (multiple-value-bind (value found) (gethash node *cache*)
    (if found
        value
        (setf (gethash node *cache*) (calculate-node-value node)))))


(defun calculate-node-value (node)
  (if (zerop (node-child-count node))
      (apply #'+ (node-metadata node))
      (loop for md in (node-metadata node)
         when (<= md (node-child-count node))
         summing (node-value (elt (node-children node) (1- md))))))

(defun out (obj &optional what)
  (if what
      (format t "~a: ~a~%" what obj)
      (format t "~a~%" obj)))

(with-open-file (in "08.txt")
  (let ((tree (parse-node in)))
    ;; (out tree "tree")
    (out (sum-metadata tree) "sum of metadata")
    (out (node-value tree) "value of root")))
