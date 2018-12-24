(in-package :cl-user)
(defpackage :aoc16
  (:use :cl :cl-user))
(in-package :aoc16)

(defun make-register (a b c d)
  (vector a b c d))

(defun register-read (register idx)
  (aref register idx))

(defun register-replace-value (register idx value)
  (let ((new-register (copy-seq register)))
    (setf (aref new-register idx) value)
    new-register))

;; Instructions
(defun make-reader (option var-name)
  (ecase option
    (:register `(register-read register ,var-name))
    (:value `,var-name)))

(defparameter *opcodes* nil)
(defmacro define-opcode (name op source-a source-b &optional (transform-result-fn #'identity))
  (let ((fng (gensym)))
    `(let ((,fng (defun ,name (register a b c)
                   (register-replace-value
                    register
                    c
                    (funcall ,transform-result-fn 
                             (funcall ,op ,(make-reader source-a 'a)
                                      ,(make-reader source-b 'b)))))))
       (setf *opcodes* (append *opcodes* (list ,fng))))))

(define-opcode addr #'+ :register :register)
(define-opcode addi #'+ :register :value)

(define-opcode mulr #'* :register :register)
(define-opcode muli #'* :register :value)

(define-opcode banr #'logand :register :register)
(define-opcode bani #'logand :register :value)

(define-opcode borr #'logior :register :register)
(define-opcode bori #'logior :register :value)

(flet ((ignore-b (a b) (declare (ignore b)) a))
  (define-opcode setr #'ignore-b :register :value)
  (define-opcode seti #'ignore-b :value :value))

(flet ((transform-bool (bool) (if bool 1 0)))
  (define-opcode gtir #'> :value :register #'transform-bool)
  (define-opcode gtri #'> :register :value #'transform-bool)
  (define-opcode gtrr #'> :register :register #'transform-bool)

  (define-opcode eqir #'= :value :register #'transform-bool)
  (define-opcode eqri #'= :register :value #'transform-bool)
  (define-opcode eqrr #'= :register :register #'transform-bool))

;; Problem
(defun sample-in (sample) (first sample))
(defun sample-out (sample) (sixth sample))
(defun sample-op (sample) (second sample))
(defun sample-args (sample) (list (third sample) (fourth sample) (fifth sample)))

(defun sample-matches-opcode-p (sample opcode)
  (equalp (apply opcode (sample-in sample)
                 (sample-args sample))
          (sample-out sample)))

(defun operations-matching-sample (sample)
  (remove-if #'null (mapcar (lambda (opcode)
                              (when (sample-matches-opcode-p sample opcode)
                                opcode))
                            *opcodes*)))
  
(defun count-opcodes (register-in op a b c register-out)
  (count t (mapcar (lambda (opcode)
            (sample-matches-opcode-p (list register-in op a b c register-out) opcode))
          *opcodes*)))

(defun samples-matching-3 (samples)
  (count-if (lambda (c) (<= 3 c))
            (mapcar #'length
                    (mapcar #'operations-matching-sample samples))))

(defun create-loose-mapping-from-samples (samples)
  (loop
     with possible-operations = (mapcar #'operations-matching-sample samples)
     with mapping = (make-array 16 :initial-element nil)
     for sample in samples
     for operations in possible-operations
     do (setf (aref mapping (sample-op sample))
              (remove-duplicates (append (aref mapping (sample-op sample)) operations)))
     finally (return mapping)))

(defun create-mapping-from-samples (samples)
  (let ((loose-mapping (create-loose-mapping-from-samples samples)))
    (labels ((dfs (opcode &optional acc)
               (if (= 16 opcode)
                   (apply #'vector (reverse acc))
                   (loop
                      for operation in (set-difference (aref loose-mapping opcode) acc)
                      for result = (dfs (1+ opcode) (cons operation acc))
                      then (dfs (1+ opcode) (cons operation acc))

                      until (= (length result) 16)
                      finally (return result)))))
      (dfs 0))))

(defun run-program (samples program)
  (let ((operation-for-opcode (create-mapping-from-samples samples)))
    (reduce (lambda (registers instruction)
              (apply (aref operation-for-opcode (first instruction))
                     registers (rest instruction)))
            program
            :initial-value (make-register 0 0 0 0))))

;; Reading the input
(defun clean-line (line)
  (substitute-if #\Space (lambda (c) (find c "[],")) line))

(defun parse-register (line)
  (with-open-stream (stream (make-string-input-stream (clean-line line)))
    (loop for _ from 0 below (length "Before: [") do (read-char stream))
    (apply
     #'make-register
     (loop for register-value = (read stream nil)
        while register-value
        collect register-value))))

(defun sample-p (stream)
  (char= (peek-char nil stream nil nil) #\B))

(defun parse-sample (stream)
  "Read a sample from a stream. A sample ends with an empty line."
  (when (sample-p stream)
    (let ((sample (list
                   (parse-register (read-line stream))
                   (read stream) (read stream) (read stream) (read stream)
                   (parse-register (read-line stream)))))
      (read-line stream nil)
      sample)))

(defun parse-samples (stream)
  (loop for sample = (parse-sample stream)
     while sample
     collect sample))

(defun parse-instruction (line)
  (with-open-stream (stream (make-string-input-stream line))
    (list (read stream) (read stream) (read stream) (read stream))))

(defun parse-program (stream)
  (loop for line = (read-line stream nil)
     while line
     when (< 0 (length line))
     collect (parse-instruction line)))

(defun parse-input (file)
  (with-open-file (stream file)
    (list (parse-samples stream) (parse-program stream))))

(defun input-samples (input) (first input))
(defun input-program (input) (second input))

;; Results

(time (format t "~a~%" (samples-matching-3 (input-samples (parse-input "16-test.txt")))))

(let ((input (parse-input "16.txt")))
  (time (format t "~a~%" (samples-matching-3 (input-samples input))))
  (time (format t "~a~%" (run-program (input-samples input) (input-program input)))))
