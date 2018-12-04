(defstruct rect
  id
  top
  left
  width
  height)

(defstruct coord
  x
  y)

(defun read-int (stream)
  (parse-integer 
   (concatenate 'string
                (loop for chr = (peek-char nil stream nil nil)
                   while (and chr (digit-char-p chr))
                   do (read-char stream)
                   collecting chr))))

(defun parse-line (line)
  (with-open-stream (s (make-string-input-stream line))
    (let ((pound (read-char s nil))
          (id (read-int s))
          (at (repeat 3 #'read-char s nil))
          (left (read-int s))
          (comma (read-char s nil))
          (top (read-int s))
          (colon (repeat 2 #'read-char s nil))
          (width (read-int s))
          (x (read-char s nil))
          (height (read-int s)))
      (declare (ignore pound at comma colon x))
      (make-rect :id id :top top :left left :width width :height height))))

(defun repeat (times fn &rest args)
  (loop for i from 0 below times
     for res = (apply fn args)
     finally (return res)))

(defun make-coord-from-rect (rect offset-x offset-y)
  (make-coord
   :x (+ (rect-left rect) offset-x)
   :y (+ (rect-top rect) offset-y)))

(defun hash-table-values (hash-table)
  (loop for value being the hash-values of hash-table
     collect value))

(defun rect-for-each-coord-do (rect fn)
  (loop for offset-x from 0 below (rect-width rect)
     do (loop for offset-y from 0 below (rect-height rect)
           do (funcall fn
                       (+ (rect-left rect) offset-x)
                       (+ (rect-top rect) offset-y)))))

(defun no-overlap-p (rect grid)
  (let ((overlap-p nil))
    (rect-for-each-coord-do rect
                            (lambda (x y)
                              (when (< 1 (gethash (make-coord :x x :y y) grid))
                              	(setf overlap-p t))))
    (not overlap-p)))

(with-open-file (f "03.txt")
  (let ((rects (loop for line = (read-line f nil)
                  while line
                  collect (parse-line line))))
    (let ((grid (make-hash-table :test 'equalp)))
      (loop for rect in rects
         do (rect-for-each-coord-do rect (lambda (x y)
                                           (incf (gethash (make-coord :x x :y y) grid 0)))))

      (format t "Overlapping squares: ~a~%"
              (count-if (lambda (val) (< 1 val))
                        (hash-table-values grid)))

      (format t "Non overlapping rect: ~a~%"
              (find-if (lambda (rect) (no-overlap-p rect grid)) rects)))))

