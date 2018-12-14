(defmacro with-string-input-stream (var line &body body)
  `(with-open-stream (,var (make-string-input-stream ,line))
     ,@body))

(defun get-transformation (transformations in)
  (gethash in transformations nil))

(defun add-transformation (transformations in out)
  (setf (gethash in transformations) out))

(defun build-transformations (transformation-list)
  (loop
     with transformations = (make-hash-table :test 'equalp)
     for transformation in transformation-list
     do (setf (gethash (first transformation) transformations) (second transformation))
     finally (return transformations)))

(defun state-to-bool (state)
  (if (char= state #\#) t nil))

(defun bool-to-state (bool)
  (if bool #\# #\.))

(defun parse-transformation (line)
  (with-string-input-stream stream line
    (let ((in (mapcar #'state-to-bool (loop for _ from 0 below 5 collect (read-char stream))))
          (junk (loop for _ from 0 below 4 do (read-char stream)))
          (out (state-to-bool (read-char stream))))
      (declare (ignore junk))
      (list in out))))

(defun plant-p (plants index)
  (gethash index plants nil))

(defun plant-in-next-gen-p (current-gen index transformations)
  (let ((surroundings (mapcar (lambda (idx)
                                (plant-p current-gen idx))
                              (loop for i from (- index 2) upto (+ index 2) collecting i))))
    (get-transformation transformations surroundings)))

(defun first-plant-index (plants)
  (loop for index being the hash-keys in plants minimizing index))

(defun last-plant-index (plants)
  (loop for index being the hash-keys in plants maximizing index))

(defun add-plant (plants index)
  (setf (gethash index plants) t))

(defun next-generation (current-gen transformations)
  (let ((gen (make-hash-table)))
    (loop for plant-index from (- (first-plant-index current-gen) 2) upto (+ (last-plant-index current-gen) 2)
       when (plant-in-next-gen-p current-gen plant-index transformations)
       do (add-plant gen plant-index))
    gen))

(defun build-generation (plant-list)
  (loop
     with plants = (make-hash-table)
     for idx from 0 below (length plant-list)
     for plant in plant-list
     when plant
     do (add-plant plants idx)
     finally (return plants)))


(defun serialize-plants (plants)
  (concatenate 'string
               (mapcar #'bool-to-state (loop for plant-index from (- (first-plant-index plants) 2) upto (+ (last-plant-index plants) 2)
                                          collect (plant-p plants plant-index)))))

(defun parse-initial-state (line)
  (with-string-input-stream stream line
    (let ((junk (loop for _ from 0 below (length "initial state: ") do (read-char stream)))
          (state (mapcar #'state-to-bool (loop for c = (read-char stream nil) while c collecting c))))
      (declare (ignore junk))
      state)))

(defun parse-input (file-name)
  (with-open-file (file file-name)
    (let ((lines (loop for line = (read-line file nil)
                    while line
                    collect line)))
      (list
       (build-generation (parse-initial-state (first lines)))
       (build-transformations (loop for line in (rest (rest lines)) collect (parse-transformation line)))))))

(defun generation-n (n initial transformations)
  (loop for i from 0 upto n
     for gen = initial then (next-generation gen transformations)
     finally (return gen)))

(defun score-generation (plants)
  (loop for index being the hash-keys in plants sum index))

(defun find-cycle (initial transformations)
  (let ((seen (make-hash-table :test 'equal)))
    (flet ((seen? (config) (gethash config seen nil))
           (seen! (config generation score) (setf (gethash config seen) (list generation score)))
           (first-score (config) (second (gethash config seen)))
           (first-seen (config) (first (gethash config seen))))
      (loop
         for generation = 0 then (1+ generation)
         for plants = initial then (next-generation plants transformations)
         for config = (serialize-plants plants) then (serialize-plants plants)

         when (seen? config)
         do (return (list config (first-seen config) (first-score config) generation (score-generation plants)))

         unless (seen? config)
         do (seen! config generation (score-generation plants))))))

(defun calculate-score-of-high-generation (n initial transformations)
  (multiple-value-bind (plants first-seen first-score second-seen second-score)
      (values-list (find-cycle initial transformations))
    (declare (ignore plants))
    (assert (< second-seen n))
    (assert (= (- second-seen 1) first-seen))
    (let ((step (- second-score first-score))
          (remaining-generations (- n second-seen)))
      (+ second-score (* step remaining-generations)))))

(defun solve ()
  (let ((input (parse-input "12-test.txt")))
    (format t "~a~%" (serialize-plants (generation-n 0 (first input) (second input))))
    (format t "~a~%" (serialize-plants (generation-n 1 (first input) (second input))))
    (format t "~a~%" (serialize-plants (generation-n 2 (first input) (second input))))
    (format t "score after 20 generations: ~a~%" (score-generation (generation-n 20 (first input) (second input))))
    (format t "cycle: ~a~%" (find-cycle (first input) (second input)))
    (format t "score after 50000000000 generations: ~a~%" (calculate-score-of-high-generation 50000000000 (first input) (second input)))))

(time (solve))
