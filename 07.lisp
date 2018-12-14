(defstruct graph incoming outgoing)

(defun min-by (list selector)
  (when list
    (loop
       with min-elt = (first list)
       with min-val = (funcall selector (first list))
       for elt in list
       when (< (funcall selector elt) min-val)
       do (setf
           min-elt elt
           min-val (funcall selector elt))
       finally (return min-elt))))

(defun graph-vertices (graph)
  (loop for vertex being the hash-keys in (graph-incoming graph)
     collect vertex))

(defun graph-incoming-edges (graph vertex)
  (gethash vertex (graph-incoming graph) nil))

(defun graph-outgoing-edges (graph vertex)
  (gethash vertex (graph-outgoing graph) nil))

(defun ensure-key (hash key)
  (multiple-value-bind (_ found)
      (gethash key hash)
    (declare (ignore _))
    (unless found (setf (gethash key hash) nil))))

(defun graph-ensure-vertex (graph vertex)
  (ensure-key (graph-incoming graph) vertex)
  (ensure-key (graph-outgoing graph) vertex))

(defun graph-add-edge (graph outgoing incoming)
  (graph-ensure-vertex graph outgoing)
  (graph-ensure-vertex graph incoming)

  (setf (gethash outgoing (graph-outgoing graph))
        (cons incoming (gethash outgoing (graph-outgoing graph))))

  (setf (gethash incoming (graph-incoming graph))
        (cons outgoing (gethash incoming (graph-incoming graph)))))

(defun graph-remove-edge (graph outgoing incoming)
  (setf (gethash outgoing (graph-outgoing graph))
        (remove incoming (graph-outgoing-edges graph outgoing)))

  (setf (gethash incoming (graph-incoming graph))
        (remove outgoing (graph-incoming-edges graph incoming))))


(defun parse-line (line)
  (with-open-stream (stream (make-string-input-stream line))
    (let ((junk1 (loop for _ from 0 below (length "Step ") do (read-char stream)))
          (incoming (read-char stream))
          (junk2 (loop for _ from 0 below (length " must be finished before step ") do (read-char stream)))
          (outgoing (read-char stream)))
      (declare (ignore junk1 junk2))
      (list incoming outgoing))))

(defun parse-input (stream)
  (build-graph (loop for line = (read-line stream nil)
                  while line
                  collect (parse-line line))))

(defun build-graph (edges)
  (loop
     with graph = (make-graph :incoming (make-hash-table) :outgoing (make-hash-table))
     for edge in edges
     do (graph-add-edge graph (first edge) (second edge))
     finally (return graph)))

(defun kahn-start-nodes (graph)
  (loop for vertex in (graph-vertices graph)
     when (null (graph-incoming-edges graph vertex))
     collect vertex))

(defun kahn (graph)
  ;; L ← Empty list that will contain the sorted elements
  ;; S ← Set of all nodes with no incoming edge
  ;; while S is non-empty do
  ;;     remove a node n from S
  ;;     add n to tail of L
  ;;     for each node m with an edge e from n to m do
  ;;         remove edge e from the graph
  ;;         if m has no other incoming edges then
  ;;             insert m into S
  ;; if graph has edges then
  ;;     return error   (graph has at least one cycle)
  ;; else 
  ;;     return L   (a topologically sorted order)
  (with-output-to-string (L)
    (loop
       with S = (kahn-start-nodes graph)
       until (null S)
       do (progn
            (let ((n (min-by S #'char-code)))
              (setf S (remove n S))
              (write-char n L)
              (loop for m in (graph-outgoing-edges graph n)
                 do (progn
                      (graph-remove-edge graph n m)
                      (when (null (graph-incoming-edges graph m))
                        (setf S (cons m S))))))))))

;; Simple queue. Not very efficient, but it'll do
(defun make-queue () nil)
(defun queue-length (queue) (length queue))
(defun queue-empty-p (queue) (null queue))
(defun enqueue (queue item) (append queue (list item)))
(defun dequeue (queue) (rest queue))
(defun queue-front (queue) (first queue))

(defun worker-queue-full-p (queue worker-count)
  (<= worker-count (queue-length queue)))

(defun make-item (name current-time penalty)
  (list name (+ current-time (- (char-code name) (char-code #\A)) penalty 1)))

(defun pull-into-queue (queue worker-count S current-time penalty)
  (loop
     with new-queue = queue
     with new-S = S
     until (or (null new-S)
               (worker-queue-full-p new-queue worker-count))

     do
     ;; add the lowest item to the worker queue... in order to do that, we need to know the current time!
       (let ((n (min-by new-S #'char-code)))
         ;; n is the next item
         (setf
          new-queue (enqueue new-queue (make-item n current-time penalty))
          new-S (remove n new-S)))
     finally (return (list new-queue new-S))))

(defun take-from-queue (queue)
  (let ((next-time (apply #'min (mapcar #'second queue))))
    (list
     (remove-if-not (lambda (item) (= (second item) next-time)) queue) ; dequeued items
     (remove-if (lambda (item) (= (second item) next-time)) queue)))) ; remaining items


(defun kahn2 (graph worker-count penalty)
  ;; instead of taking directly from S,
  ;; insert a queue in between: a worker queue.
  ;; we pull as much as we can from S into the worker queue
  ;; then pull from the worker queue and insert new jobs into S.
  ;;
  ;; at every iteration we pull as much as we can from S into the worker queue
  ;; and we remove all jobs for second n.
  ;;
  ;; speaking of queue: we need to implement a simple queue..
  (let* ((current-time 0)
         (order (with-output-to-string (L)
                  (loop
                     with S = (kahn-start-nodes graph)
                     with work-queue = (make-queue)

                     until (and (null S) (queue-empty-p work-queue))

                     do (progn
                          (multiple-value-bind (new-work-queue new-S) (values-list (pull-into-queue work-queue worker-count S current-time penalty))
                            (setf S new-S
                                  work-queue new-work-queue))
                          (multiple-value-bind (finished remaining) (values-list (take-from-queue work-queue))
                            (setf work-queue remaining)
                            (loop for item in finished
                               do (let ((n (first item))
                                        (time (second item)))
                                    (setf current-time time)
                                    (write-char n L)
                                    (loop for m in (graph-outgoing-edges graph n)
                                       do (progn
                                            (graph-remove-edge graph n m)
                                            (when (null (graph-incoming-edges graph m))
                                              (setf S (cons m S)))))))))))))
    (list current-time order)))



(with-open-file (in "07.txt")
  (let ((graph (parse-input in)))
    (format t "~a~%" (kahn graph))))

(with-open-file (in "07.txt")
  (let ((graph (parse-input in)))
    (format t "~a~%" (kahn2 graph 5 60))))
