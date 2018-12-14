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

;; Given
;;  A B C
;;  D E F
;;  G H I
;; 
;; The partial sum of I is
;;   PS[H] + PS[F] + power(I) - PS[E]
;;     The reason for - PS[E] is because we've counted that twice
(defun ps (arr x y)
  (aref arr x y))

(defun partial-sums (grid-serial-number)
  (loop
     with partials = (make-array (list 301 301))
     for x from 1 upto 300
     do (loop for y from 1 upto 300
           do (setf (aref partials x y)
                    (-
                     (+ (ps partials (1- x) y)
                        (ps partials x (1- y))
                        (power-value x y grid-serial-number))
                     (ps partials (1- x) (1- y)))))
     finally (return partials)))

;; Given
;;  A B C D E
;;  F G H I J
;;  K L M N O
;;  P Q R S T
;;  U V W X Y
;;
;; The power value of a 3x3 grid at G is...
;;   PS[S] - PS[D] - PS[P] + PS[A]
(defun grid-value (partials x y grid-size)
  (+
   (- (ps partials (+ x grid-size -1) (+ y grid-size -1))
      (ps partials (1- x) (+ y grid-size -1))
      (ps partials (+ x grid-size -1) (1- y)))
   (ps partials (1- x) (1- y))))

(defun find-highest-power-grid (grid-serial-number grid-size &optional (partials (partial-sums grid-serial-number)))
  (loop
     with best-power = 0
     with best-coord = nil

     for x from 1 upto (1+ (- 300 grid-size))
     do (loop for y from 1 upto (1+ (- 300 grid-size))
           when (< best-power (grid-value partials x y grid-size))
           do (setf best-power (grid-value partials x y grid-size)
                    best-coord (list x y)))

     finally (return (list best-power best-coord))))

(defun find-highest-power-grid-of-any-size (grid-serial-number)
  (loop
     with partials = (partial-sums grid-serial-number)
     with best-power = 0
     with best-coord = nil

     for grid-size from 1 to 300

     for result = (find-highest-power-grid grid-serial-number grid-size partials)
     then (find-highest-power-grid grid-serial-number grid-size partials)

     when (< best-power (first result))
     do (setf best-power (first result)
              best-coord (list (second result) grid-size))

     finally (return (list best-power best-coord))))

(format t "coord: ~a~%" (find-highest-power-grid 42 3))
(format t "coord: ~a~%" (find-highest-power-grid 8868 3))

(time (format t "coord: ~a~%" (find-highest-power-grid-of-any-size 18)))
(format t "coord: ~a~%" (find-highest-power-grid-of-any-size 42))
(time (format t "coord: ~a~%" (find-highest-power-grid-of-any-size 8868)))
