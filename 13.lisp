(defun track-piece (tracks x y)
  (aref (aref tracks y) x))

(defun cart-p (piece)
  (and (find piece "^>v<") t))

(defun make-cart (x y piece-or-direction &optional (next-turn :left))
  (if (eq (type-of piece-or-direction) 'standard-char)
      (list x y (search (string piece-or-direction) "^>v<") next-turn)
      (list x y piece-or-direction next-turn)))

(defun cart-x (cart) (first cart))
(defun cart-y (cart) (second cart))
(defun cart-direction (cart) (third cart))
(defun cart-next-turn (cart) (fourth cart))

(defun find-carts (tracks)
  (loop
     for y = 0 then (1+ y)
     for row across tracks
     nconc (loop
              for x = 0 then (1+ x)
              for piece across row
              when (cart-p piece)
              collect (make-cart x y piece))))

(let ((dir-x (vector  0 1 0 -1))
      (dir-y (vector -1 0 1  0)))
  (defun dx (direction) (aref dir-x direction))
  (defun dy (direction) (aref dir-y direction)))

(defun turn-left (direction)
  (mod (1- direction) 4))

(defun turn-right (direction)
  (mod (1+ direction) 4))

(defun intersection-p (piece)
  (char= piece #\+))

(defun turn-p (piece)
  (or (char= piece #\\) (char= piece #\/)))

(defun perform-intersection (cart)
  (ecase (cart-next-turn cart)
    (:left (make-cart (cart-x cart) (cart-y cart) (turn-left (cart-direction cart)) :straight))
    (:straight (make-cart (cart-x cart) (cart-y cart) (cart-direction cart) :right))
    (:right (make-cart (cart-x cart) (cart-y cart) (turn-right (cart-direction cart)) :left))))

(defun vertical-cart-piece-p (piece)
  (or (char= piece #\v) (char= piece #\^)))
(defun horizontal-cart-piece-p (piece)
  (or (char= piece #\<) (char= piece #\>)))

(defun cart-moving-horizontal-p (cart)
  (oddp (cart-direction cart)))

(defun cart-moving-vertical-p (cart)
  (not (cart-moving-horizontal-p cart)))

(defun perform-turn (cart tracks)
  (let ((current-piece (track-piece tracks (cart-x cart) (cart-y cart))))
    (make-cart
     (cart-x cart)
     (cart-y cart)
     (funcall 
      (cond
        ((and (char= current-piece #\\)
              (cart-moving-horizontal-p cart))
         #'turn-right)
        ((and (char= current-piece #\\)
              (cart-moving-vertical-p cart))
         #'turn-left)
        ((and (char= current-piece #\/)
              (cart-moving-horizontal-p cart))
         #'turn-left)
        ((and (char= current-piece #\/)
              (cart-moving-vertical-p cart))
         #'turn-right)
        (t (error cart)))
      (cart-direction cart))
     (cart-next-turn cart))))

(defun turn-cart (cart tracks)
  "Assuming cart just arived on the current position, turn it where needed:
     - if on a '+', turn to whatever direction the cart must turn this time,
       and set the next turn
     - if on a '\' or '/', check where we came from, and turn accordingly
     - otherwise, do nothing"
  (let ((piece (track-piece tracks (cart-x cart) (cart-y cart))))
    (cond ((intersection-p piece) (perform-intersection cart))
          ((turn-p piece) (perform-turn cart tracks))
          (t cart))))

(defun move-cart (cart tracks)
  (let* ((new-x (+ (cart-x cart) (dx (cart-direction cart))))
         (new-y (+ (cart-y cart) (dy (cart-direction cart))))
         (moved-cart (make-cart new-x new-y (cart-direction cart) (cart-next-turn cart))))
    (turn-cart moved-cart tracks)))

(defun sorted-carts (carts)
  ;; (format t "~a~%" carts)
  (stable-sort 
   (sort (copy-list carts) #'< :key #'cart-y)
   #'<
   :key #'cart-x))

(defun do-step (tracks carts cart-positions)
  (loop
     with sorted-carts = (sorted-carts carts)
     with new-carts = (mapcar (lambda (c) (move-cart c tracks)) sorted-carts)

     for old-cart in sorted-carts
     for new-cart in new-carts

     do (let ((old-position (list (cart-x old-cart) (cart-y old-cart)))
              (new-position (list (cart-x new-cart) (cart-y new-cart))))
          (if (gethash new-position cart-positions)
              (return (list nil new-position))
              (progn
                (remhash old-position cart-positions)
                (setf (gethash new-position cart-positions) t))))

     finally (return (list t new-carts))))

(defun make-cart-positions (carts)
  (loop with cart-positions = (make-hash-table :test 'equalp)
     for cart in carts
     do (setf (gethash (list (cart-x cart) (cart-y cart)) cart-positions) t)
     finally (return cart-positions)))

(defun simulate-until-first-crash (tracks carts)
  (loop
     with cart-positions = (make-cart-positions carts)

     for result = (do-step tracks carts cart-positions)
     then (do-step tracks (second result) cart-positions)

     while (first result)

     finally (return (second result))))

(defun collision-p (positions cart)
  (find
   (list (cart-x cart) (cart-y cart))
   positions :test #'equalp :key (lambda (c) (list (cart-x c) (cart-y c)))))

(defun remove-collisions (positions cart)
  (remove 
   (list (cart-x cart) (cart-y cart))
   positions :test #'equalp :key (lambda (c) (list (cart-x c) (cart-y c)))))


(defun do-step-removing-crashing-carts (tracks carts)
  (labels ((move-the-carts (current-positions new-positions)
             (if (null current-positions)
                 (reverse new-positions)
                 (let ((moved-cart (move-cart (first current-positions) tracks)))
                   (if (collision-p (append (rest current-positions) new-positions)
                                    moved-cart)
                       (move-the-carts
                        (remove-collisions (rest current-positions) moved-cart)
                        (remove-collisions new-positions moved-cart))
                       (move-the-carts (rest current-positions)
                                       (cons moved-cart new-positions)))))))
    (move-the-carts (sorted-carts carts) nil)))

(defun simulate-until-one-cart-remains (tracks carts)
  (loop
     for remaining-carts = (do-step-removing-crashing-carts tracks carts)
     then (do-step-removing-crashing-carts tracks remaining-carts)

     while (< 1 (length remaining-carts))

     finally (return remaining-carts)))

(defun parse-input (file-name)
  (with-open-file (file file-name)
    (apply #'vector (loop for line = (read-line file nil)
                       while line
                       collect line))))

(defun print-tracks (tracks)
  (loop for line across tracks
     do (format t "~a~%" line)))

(defun solve (file-name)
  (let ((tracks (parse-input file-name)))
    (format t "~a~%" (track-piece tracks 2 0))
    (format t "~a~%" (sort-carts (reverse (find-carts tracks))))
    ;; (print-tracks tracks)
    (format t "first crash: ~a~%"
            (simulate-until-first-crash tracks (find-carts tracks)))
    (format t "final cart: ~a~%"
            (simulate-until-one-cart-remains tracks (find-carts tracks)))))

(time (solve "13-test.txt"))
(time (solve "13-2-test.txt"))
(time (solve "13.txt"))
