(defstruct guard-shift
  guard-id
  sleep-intervals)

(defun read-int (stream)
  (parse-integer 
   (concatenate 'string
                (loop for chr = (peek-char nil stream nil nil)
                   while (and chr (digit-char-p chr))
                   do (read-char stream)
                   collecting chr))))

(defun parse-guard-id (line)
  ;; eat the first 26 characters, then read the next int
  (with-open-stream (stream (make-string-input-stream line))
    (loop for i from 0 below 26 do (read-char stream))
    (read stream)))

(defun parse-minutes (line)
  ;; eat the first 14 characters, then read the next int
  (with-open-stream (stream (make-string-input-stream line))
    (loop for i from 0 below 15 do (read-char stream))
    (read-int stream)))

(defun parse-sleep-interval (fall-asleep wake-up)
  (list (parse-minutes fall-asleep) (parse-minutes wake-up)))

(defun parse-sleep-intervals (lines)
  (labels ((parse-sleep-intervals-recur (lines acc)
             (if (null lines)
                 (reverse acc)
                 (parse-sleep-intervals-recur
                  (rest (rest lines))
                  (cons (parse-sleep-interval (first lines) (second lines))
                        acc)))))
    (parse-sleep-intervals-recur lines nil)))


(defun parse-shift (lines)
  ;; we don't care about the first line.
  ;; we don't care about the date

  ;; in my input, every guard always wakes up.
  ;;   (assert (oddp (length lines)))
  ;; so we can parse the minutes out of every pair of lines
  ;; and note those as sleep intervals
  (let ((guard-id (parse-guard-id (first lines)))
        (sleep-intervals (parse-sleep-intervals (rest lines))))
    (make-guard-shift :guard-id guard-id :sleep-intervals sleep-intervals)))

(defun split-input (lines)
  ;; look at current line
  ;; if it contains "Guard" start a new collection
  ;; otherwise, add to current collection
  (labels ((split-input-recur (remaining-lines current-guard all)
             (if (null remaining-lines)
                 (cons (reverse current-guard) all)
                 (let ((line (first remaining-lines)))
                   (if (search "Guard" line)
                       (split-input-recur (rest remaining-lines)
                                          (list line)
                                          (cons (reverse current-guard) all))
                       (split-input-recur (rest remaining-lines)
                                          (cons line current-guard)
                                          all))))))
    (rest (reverse (split-input-recur lines nil nil)))))

(defun parse-input (lines)
  (mapcar (lambda (shift) (parse-shift shift))
          (split-input (sort lines #'string<))))

(defun read-input (stream)
  (loop for line = (read-line stream nil)
     while line
     collect line))

;; Strategy 1
;; Find the guard that has the most minutes asleep
;; For this guard, find the minute that he has been asleep the most
;; ID of guard * minute
(defun guard-shift-minutes-asleep (shift)
  (loop for interval in (guard-shift-sleep-intervals shift)
     sum (- (second interval) (first interval))))

(defun sum-minutes-asleep (shifts)
  (let ((sleepy (make-hash-table)))
    (loop for shift in shifts
       do (incf (gethash (guard-shift-guard-id shift) sleepy 0)
                (guard-shift-minutes-asleep shift)))
    sleepy))

(defun get-key-of-highest-value (hash-table)
  (let ((best-val 0)
        (best-key nil))
    (loop for key being the hash-keys in hash-table
       for value being the hash-values in hash-table
       when (< best-val value)
       do (setf best-key key
                best-val value))
    best-key))

(defun most-sleepy-guard (shifts)
  (let ((sleepy (sum-minutes-asleep shifts)))
    (get-key-of-highest-value sleepy)))

(defun most-slept-minute (guard-id shifts)
  (let ((minutes (make-hash-table)))
    (loop for shift in shifts
       when (= guard-id (guard-shift-guard-id shift))
       do (loop for interval in (guard-shift-sleep-intervals shift)
             do (loop for minute from (first interval) below (second interval)
                   do (incf (gethash minute minutes 0)))))
    (get-key-of-highest-value minutes)))

(defun strategy-one (shifts)
  (let* ((sleepy-guard (most-sleepy-guard shifts))
         (sleepy-minute (most-slept-minute sleepy-guard shifts)))
    (* sleepy-guard sleepy-minute)))

;; Strategy two
;; Find each combination of minute/guard
;; Find the one that occurs the most
;; Multiply the minute and the guard id
(defun minute-slept-per-guard (shifts)
  (let ((combinations (make-hash-table :test 'equalp)))
    (loop for shift in shifts
       do (loop for interval in (guard-shift-sleep-intervals shift)
             do (loop for minute from (first interval) below (second interval)
                   do (incf (gethash
                             (list (guard-shift-guard-id shift) minute)
                             combinations
                             0)))))
    combinations))

(defun strategy-two (shifts)
  (let* ((sleepy-minutes-per-guard (minute-slept-per-guard shifts))
         (best (get-key-of-highest-value sleepy-minutes-per-guard)))
    (* (first best) (second best))))

(with-open-file (in "04.txt")
  (let ((shifts (parse-input (read-input in))))
    ;; (format t "shifts: ~a~%" shifts)
    (format t "strategy one: ~a~%"
            (strategy-one shifts))
    (format t "strategy two: ~a~%"
            (strategy-two shifts))))
