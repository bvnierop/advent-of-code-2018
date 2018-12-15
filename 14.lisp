(defun make-score-list ()
  (make-array 2 :element-type 'integer :adjustable t :initial-contents (list 3 7) :fill-pointer 2))

(defun digits (num)
  (if (zerop num)
      (list 0)
      (labels ((extract-digits (num acc)
                 (if (zerop num)
                     acc
                     (extract-digits (truncate num 10) (cons (mod num 10) acc)))))
        (extract-digits num nil))))

(defun add-to-score-list (score-list new-score)
  (vector-push-extend new-score score-list))

(defun score-for (score-list index)
  (aref score-list index))

(defun next-index (score-list current-index)
  (mod
   (+ (score-for score-list current-index)
      current-index
      1)
   (length score-list)))

(defun perform-round (elf-one elf-two score-list)
  (loop for digit in (digits
                      (+ (score-for score-list elf-one)
                         (score-for score-list elf-two)))
     do (vector-push-extend digit score-list))
  (list (next-index score-list elf-one)
        (next-index score-list elf-two)
        score-list))

(defun collect-result (score-list n)
  (loop for i from n below (+ n 10)
     collect (score-for score-list i)))

(defun run-rounds (n &optional (score-list (make-score-list)))
  (labels ((run-rounds-rec (required elf-one elf-two)
             (if (<= required (length score-list))
                 (list elf-one elf-two score-list)
                 (let ((round-result (perform-round elf-one elf-two score-list)))
                   (run-rounds-rec required
                                   (first round-result)
                                   (second round-result))))))
    (run-rounds-rec (+ n 10) 0 1)))

(defun gen-until-found (n)
  (loop
     with to-find = (apply #'vector (digits n))
     for sz = 1 then (* sz 2)
     for score-list = (run-rounds sz) then (run-rounds sz)
     for result = (search to-find (third score-list)) then (search to-find (third score-list))
     until result
     finally (return result)))


(defun solve-part-one (input)
  (format t "~a~%" input)
  (format t "~{~a~}~%" (collect-result (third (run-rounds input)) input)))

(time (solve-part-one 9)) ;; 5158916779
(time (solve-part-one 5)) ;; 0124515891
(time (solve-part-one 18)) ;; 9251071085
(time (solve-part-one 2018)) ;; 5941429882
(time (solve-part-one 607331))

(defun solve-part-two (input)
  (format t "~a~%" input)
  (format t "~a~%" (gen-until-found input)))

(time (solve-part-two 51589)) ;; 9 recipes.
(time (solve-part-two 01245)) ;;5 recipes.
(time (solve-part-two 92510)) ;; 18 recipes.
(time (solve-part-two 59414)) ;; 2018 recipes. 
(time (solve-part-two 607331))
