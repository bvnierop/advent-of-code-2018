(defun memoize (fn)
  (let ((cache (make-hash-table :test 'equalp)))
    (lambda (&rest args)
      (multiple-value-bind (value found) (gethash args cache)
        (if found
            (progn
              (format t "Found cache for args: ~a: ~a~%" args value)
              value)
            (setf (gethash args cache) (apply fn args)))))))

(defun rack-id (x y)
  (declare (ignore y))
  (+ x 10))

(defun initial-power-value (x y)
  (* (rack-id x y) y))

(defun hundreds-digit (num)
  (rem (floor num 100) 10))

(defun power-value (x y grid-serial-number)
  (-
   (hundreds-digit (*
                    (+
                     (initial-power-value x y)
                     grid-serial-number)
                    (rack-id x y)))
   5))

;; solve each 1x1
;; for 2x2: 1x1 + 1x1's
;; for 3x3: 2x2 + 1x1's
;; ...

(defparameter *cache* (make-hash-table :test 'equalp))
(defun grid-value (x y grid-serial-number grid-size)
  (if (= 1 grid-size)
      (power-value x y grid-serial-number)
      (multiple-value-bind (value found) (gethash (list x y grid-serial-number grid-size) *cache*)
        (if found
            value
            (let ((result (+ (grid-value x y grid-serial-number (1- grid-size))
                             (loop for xx from x below (+ x grid-size)
                                sum (power-value xx (+ y grid-size -1) grid-serial-number))
                             (loop for yy from y below (+ y grid-size -1) ;; don't count the corner twice
                                sum (power-value (+ x grid-size -1) yy grid-serial-number)))))
              (setf (gethash (list x y grid-serial-number grid-size) *cache*) result)
              ;; remove smaller from cache, we don't need it anymore
              (remhash (list x y grid-serial-number (1- grid-size)) *cache*)
              result)))))

(defun find-highest-power-grid (grid-serial-number grid-size)
  (loop
     with best-power = 0
     with best-coord = nil

     for x from 1 upto (1+ (- 300 grid-size))
     do (loop for y from 1 upto (1+ (- 300 grid-size))
           when (< best-power (grid-value x y grid-serial-number grid-size))
           do (setf best-power (grid-value  x y grid-serial-number grid-size)
                    best-coord (list x y)))

     finally (return (list best-power best-coord))))

(defun find-highest-power-grid-of-any-size (grid-serial-number)
  (loop
     with best-power = 0
     with best-coord = nil

     for grid-size from 1 to 300

     for result = (find-highest-power-grid grid-serial-number grid-size)
     then (find-highest-power-grid grid-serial-number grid-size)

     when (< best-power (first result))
     do (setf best-power (first result)
              best-coord (list (second result) grid-size))

     finally (return (list best-power best-coord))))

(format t "coord: ~a~%" (find-highest-power-grid 42 3))
(format t "coord: ~a~%" (find-highest-power-grid 8868 3))

(format t "coord: ~a~%" (find-highest-power-grid-of-any-size 8868))
