(defun average (list &key (key #'identity))
  (when list
    (loop for elt in list
       sum (funcall key elt) into total
       finally (return (/ total (length list))))))

(defun clean-line (line)
  (substitute
   #\; #\>
   (substitute #\; #\, line)))

(defun parse-line (line)
  (with-open-stream (stream (make-string-input-stream (clean-line line)))
    (let ((junk (loop for _ from 0 below (length "position=<") do (read-char stream)))
          (x (read stream))
          (junk2 (read-char stream))
          (y (read stream))
          (junk3 (loop for _ from 0 below (length "> velocity=<") do (read-char stream)))
          (dx (read stream))
          (junk4 (read-char stream))
          (dy (read stream)))
      (declare (ignore junk junk2 junk3 junk4))
      (list x y dx dy))))

(defun px (light) (first light))
(defun py (light) (second light))
(defun dx (light) (third light))
(defun dy (light) (fourth light))

(defun mean-velocity (lights)
  (ceiling (average (remove-if (lambda (light) (zerop (dy light))) lights)
           :key (lambda (light) (abs (dy light))))))

(defun lights-with-mean-velocity (lights velocity)
  (remove-if-not
   (lambda (light)
     (= (abs (dy light)) velocity))
   lights))

(defun find-moment (lights)
  ;; we take the mean absolute y-velocity, rounded down
  ;; we take every single point with that velocity
  ;; for positive velocity, we take the smallest
  ;; for negative velocity, we take the largest
  ;; t = dist / (velo * 2)
  (let* ((relevant-lights (lights-with-mean-velocity lights (mean-velocity lights)))
         (miny (floor (average (mapcar #'py (remove-if-not #'plusp relevant-lights :key #'dy)))))
         (maxy (ceiling (average (mapcar #'py (remove-if-not #'minusp relevant-lights :key #'dy))))))
    (truncate (- maxy miny) (* (mean-velocity lights) 2))))

(defun light-after (light seconds)
  (list
   (+ (px light) (* (dx light) seconds))
   (+ (py light) (* (dy light) seconds))
   (dx light)
   (dy light)))

(defun apply-by (list &key (fn #'min) (key #'identity))
  (apply fn (mapcar key list)))


(defun generate-message (lights)
  (let* ((moment (find-moment lights))
         (positioned-lights (mapcar (lambda (light) (light-after light moment)) lights))
         (minx (apply-by positioned-lights :fn #'min :key #'px))
         (maxx (apply-by positioned-lights :fn #'max :key #'px))
         (miny (apply-by positioned-lights :fn #'min :key #'py))
         (maxy (apply-by positioned-lights :fn #'max :key #'py))
         (width (1+ (- maxx minx)))
         (height (1+ (- maxy miny)))
         (array (make-array (list height width) :initial-element #\.)))
    (loop for light in positioned-lights
       do (setf (aref array (- (py light) miny) (- (px light) minx)) #\#))
    (format nil "~{~a~%~}~%Found after ~a seconds.~%"
            (loop for row from 0 below height
               collect (concatenate 'string (loop for column from 0 below width
                                               collect (aref array row column))))
            moment)))


(defun out (obj &optional what)
  (if what
      (format t "~a: ~a~%" what obj)
      (format t "~a~%" obj)))

(with-open-file (f "10.txt")
  (out
   (generate-message
    (loop for line = (read-line f nil)
       while line
       collect (parse-line line)))))
