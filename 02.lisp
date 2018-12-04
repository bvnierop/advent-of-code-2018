(defun read-box-ids (stream)
  (loop for s = (read stream nil)
     while s
     collect (string s)))

(defun hash-table-values (hash-table)
  (loop for value being the hash-values of hash-table collect value))

(defun hash-table-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun count-elements (seq)
  (let ((counts (make-hash-table)))
    (loop for elem across seq
       do (incf (gethash elem counts 0)))
    counts))

(defun counts (seq)
  (hash-table-values
   (count-elements seq)))

(defun ids-containing-a-letter-exactly (times ids)
  (loop for id in ids
     when (find times (counts id))
     collect id))

(defun calculate-checksum (box-ids)
  (* (length (ids-containing-a-letter-exactly 2 box-ids))
     (length (ids-containing-a-letter-exactly 3 box-ids))))

(with-open-file (f "02.txt"
                   :direction :input
                   :if-does-not-exist :error)
  (format t "Checksum: ~a~%" (calculate-checksum (read-box-ids f))))

(defun amount-of-different-characters (a b)
  (loop for chr-a across a
     for chr-b across b
     counting (not (eq chr-a chr-b))))

(defun common-characters (a b)
  (concatenate 'string (loop for chr-a across a
                          for chr-b across b
                          when (eq chr-a chr-b)
                          collect chr-a)))

(defun make-pairs (seq)
  (labels ((make-pairs-rec (elem seq acc)
             (if (null seq)
                 acc
                 (make-pairs-rec elem
                                 (rest seq)
                                 (cons (list elem (first seq)) acc))))
           (make-pairs-rec-outer (seq acc)
             (if (< (length seq) 2)
                 acc
                 (make-pairs-rec-outer (rest seq)
                                       (make-pairs-rec (first seq) (rest seq) acc)))))
    (reverse (make-pairs-rec-outer seq nil))))

(with-open-file (f "02.txt"
                   :direction :input
                   :if-does-not-exist :error)
  (format t "Common characters of fabric boxes: ~a~%"
          (apply #'common-characters
                 (find-if (lambda (pair)
                            (= 1 (amount-of-different-characters (first pair) (second pair))))
                          (make-pairs (read-box-ids f))))))
