(defstruct coord x y)

(defun read-int (stream)
  (parse-integer 
   (concatenate 'string
                (loop for chr = (peek-char nil stream nil nil)
                   while (and chr (digit-char-p chr))
                   do (read-char stream)
                   collecting chr))))

(defun parse-coord (line)
  (with-open-stream (stream (make-string-input-stream line))
    (let ((x (read-int stream))
          (_ (loop for i from 0 below 2 do (read-char stream)))
          (y (read-int stream)))
      (declare (ignore _))
      (make-coord :x x :y y))))

(defun parse-input (stream)
  (loop for line = (read-line stream nil)
     while line
     collect (parse-coord line)))

(defun manhattan-distance (coord1 coord2)
  (+ (abs (- (coord-x coord1) (coord-x coord2)))
     (abs (- (coord-y coord1) (coord-y coord2)))))

(defun closest-single-point-to (x y coords)
  (let* ((target (make-coord :x x :y y))
         (smallest (apply #'min (mapcar (lambda (coord)
                                          (manhattan-distance target coord))
                                        coords)))
         (filtered (remove-if (lambda (x)
                                (not (= smallest (manhattan-distance target x))))
                              coords)))
    (if (= (length filtered) 1)
        (first filtered)
        nil)))

(defun find-areas-for (min-x max-x min-y max-y coords)
  (let ((areas (make-hash-table :test 'equalp)))
    (loop for x from min-x upto max-x
       do (loop for y from min-y upto max-y
             for coord = (closest-single-point-to x y coords) then (closest-single-point-to x y coords)
             when coord do (incf (gethash coord areas 0))))
    areas))

(defun find-largest-area (coords)
  ;; calc smallest and largest x and y
  ;; solve for an area 3 times that
  ;; solve for an area that is 1 tile larger on each side
  ;; find the largest _unchanged_ area between the two

  ;; to 'solve' means to find the area for each point
  (let* ((min-x (apply #'min (mapcar #'coord-x coords)))
         (max-x (apply #'max (mapcar #'coord-x coords)))
         (min-y (apply #'min (mapcar #'coord-y coords)))
         (max-y (apply #'max (mapcar #'coord-y coords)))
         (width (- max-x min-x))
         (height (- max-y min-y))
         (areas (find-areas-for (- min-x width) (+ max-x width)
                                (- min-y height) (+ max-y height)
                                coords))

         (areas2 (find-areas-for (- min-x width 1) (+ max-x width 1)
                                 (- min-y height 1) (+ max-y height 1)
                                 coords))
         (finite (remove-if (lambda (coord)
                              (not (= (gethash coord areas)
                                      (gethash coord areas2))))
                            coords)))

    (loop for key being the hash-keys in areas
       for val being the hash-values in areas
       when (find key finite :test #'equalp)
       maximize val)))

(defun total-distance (x y coords)
  (let ((target (make-coord :x x :y y)))
    (loop for coord in coords
       sum (manhattan-distance coord target))))

(defun area-of-safe-coords (safe-distance coords)
  (let* ((min-x (apply #'min (mapcar #'coord-x coords)))
         (max-x (apply #'max (mapcar #'coord-x coords)))
         (min-y (apply #'min (mapcar #'coord-y coords)))
         (max-y (apply #'max (mapcar #'coord-y coords)))
         (width (- max-x min-x))
         (height (- max-y min-y))
         (size 0))
    (loop for x from (- min-x width) upto (+ max-x width)
       do (loop for y from (- min-y height) upto (+ max-y height)
             when (< (total-distance x y coords) safe-distance)
             do (incf size)))
    size))
             

(with-open-file (in "06.txt")
  (let ((coords (parse-input in)))
    (format t "~a~%" coords)
    (format t "~a~%" (find-largest-area coords))
    (format t "~a~%" (area-of-safe-coords 10000 coords))))
