(defun read-changes-from-stream (stream)
  (loop for i = (read stream nil)
     while i
     collect i))

(defmacro do-for-01 (var &body body)
  `(with-open-file (f "01.txt"
                      :direction :input
                      :if-does-not-exist :error)
     (let ((,var (read-changes-from-stream f)))
       ,@body)))

(do-for-01 changes
  (format t "Resulting frequency: ~a~%"
          (apply #'+ 0 changes)))

(defun check-for-doubles (initial seen changes)
  (loop for change in changes
     for current = (+ initial change) then (+ current change)
     for seen-before = (gethash current seen) then (gethash current seen)
     until seen-before
     do (setf (gethash current seen) t)
     finally (return (values seen-before current))))

(defun check-for-doubles-until-found (changes)
  (let ((seen (make-hash-table)))
    (setf (gethash 0 seen) t)
    (loop for (found freq) = (multiple-value-list (check-for-doubles 0 seen changes))
       then (multiple-value-list (check-for-doubles freq seen changes))
       until found
       finally (return freq))))

(do-for-01 changes
  (format t "First double: ~a~%"
          (check-for-doubles-until-found changes)))
