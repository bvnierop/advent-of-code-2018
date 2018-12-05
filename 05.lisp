(defun reactive-pair-p (a b)
  (and (not (char= a b))
       (char= (char-upcase a) (char-upcase b))))

(defun collapse-reactive-pairs (str)
  (with-output-to-string (out)
    (loop
       for idx-a from 0 below (length str)
       for idx-b from 1 upto (length str)

       when (= idx-b (length str))
       do (progn (write-char (aref str idx-a) out))

       when (and (< idx-b (length str))
                 (not (reactive-pair-p (aref str idx-a) (aref str idx-b))))
       do (write-char (aref str idx-a) out)

       when (and (< idx-b (length str))
                 (reactive-pair-p (aref str idx-a) (aref str idx-b)))
       do (progn (incf idx-a 1) (incf idx-b 1)))))

(defun collapse-all-reactive-pairs (str)
  (let ((collapsed (collapse-reactive-pairs str)))
    (if (string= str collapsed)
        collapsed
        (collapse-all-reactive-pairs collapsed))))

(defun clean-str (str unit)
  (remove-if (lambda (chr)
               (char= (char-downcase chr) unit))
             str))

(defun collapse-all-reactive-pairs-after-cleaning (str)
  (loop for unit across "abcdefghijklmnopqrstuvwxyz"
     for cleaned = (clean-str str unit) then (clean-str str unit)
     for collapsed = (collapse-all-reactive-pairs cleaned) then (collapse-all-reactive-pairs cleaned)
     minimize (length collapsed)))

(with-open-file (in "05.txt")
  (let ((input (read-line in)))
    (format t "~a~%" (length (collapse-all-reactive-pairs input)))
    (format t "~a~%" (collapse-all-reactive-pairs-after-cleaning input))))
