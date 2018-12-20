;; store graph as a matrix
;; update the graph (don't generate a new one)
;;
;; FLOW
;;
;; Fight:
;;  Until the fight is over, do
;;    Sort all units in reading order
;;    For each unit
;;      Perform turn
;;    Remove dead units from list
;;
;; Turn:
;;   Move if not in combat
;;   Attack
;;
;; Move: ;; unit-move
;;   Find destinations
;;   Run BFS to find shortest paths to open squares near targets (grab all shortest paths)
;;   Sort the paths in reading order of the first step
;;   Stable sort paths to target squares in reading order
;;   Move to first destination
;;
;; Attack:
;;   Collect adjacent targets
;;   Sort in reading order
;;   Deduct 3 HP from the first one
;;   If the targets HP drops to 0 or below
;;     Remove it from the graph
;;
;; Scoring...
;;
;; What constitutes a _full_ turn?
;;  A full turn is a turn that is not ended prematurely
;; What is a prematurely ended turn?



(defun parse-file (file-name)
  (with-open-file (stream file-name)
    (apply #'vector (loop for line = (read-line stream nil)
                       until (or (not line) (zerop (length line)))
                       collect line))))

;; We need a queue for bfs. We're making an efficient, destructive queue this time.
(defstruct queue
  (head nil)
  (tail nil))

(defun queue-enqueue (queue item)
  (let ((new-cons (cons item nil)))
    (if (queue-empty-p queue)
        (setf (queue-head queue) new-cons)
        (setf (cdr (queue-tail queue)) new-cons))
    (setf (queue-tail queue) new-cons))
  queue)

(defun queue-dequeue (queue)
  (let ((head (queue-head queue)))
    (if head
        (progn
          (setf (queue-head queue) (cdr head))
          (values (car head) t))
        (values nil nil))))

(defun queue-empty-p (queue)
  (null (queue-head queue)))

(defmacro with-grid-deltas (fn-dx fn-dy fn-pairs &body body)
  "We need to do a bunch of stuff with grid deltas. All of them act the same (read order deltas).
   This macro defines some helper functions and a context in which they exist."
  (let ((gdx (gensym))
        (gdy (gensym)))
    `(let ((,gdx (vector  0 -1 1  0))
           (,gdy (vector -1  0 0  1)))
       (flet ((,fn-pairs () (loop for x across ,gdx for y across ,gdy collect (list x y)))
              (,fn-dx (direction) (aref ,gdx direction))
              (,fn-dy (direction) (aref ,gdy direction)))
         ,@body))))

(defun x (coords) (first coords))
(defun y (coords) (second coords))
(defun get-square (graph x y)
  (aref (aref graph y) x))
(defun graph-width (graph)
  (length (aref graph 0)))
(defun graph-height (graph)
  (length graph))

(defun in-graph-p (graph coords)
  (and (< -1 (x coords) (graph-width graph))
       (< -1  (y coords) (graph-height graph))))

(defun update-graph (graph coords new-value)
  (setf (aref (aref graph (y coords)) (x coords)) new-value))

(with-grid-deltas dx dy dirs
  (defun passable-p (graph coords)
    (and
     (in-graph-p graph coords)
     (char= (get-square graph (x coords) (y coords)) #\.)))

  (defun delta-coords (coords dir)
    (list (+ (x coords) (dx dir))
          (+ (y coords) (dy dir))))
  
  (defun passable-neighbours (graph coords) ;; todo: ensure coords are within limits
    (loop for dir from 0 below (length (dirs))
       for n = (delta-coords coords dir) then (delta-coords coords dir)
       when (and (in-graph-p graph n) (passable-p graph n))
       ;; when (passable-p graph n)
       collect n))

  (defun bfs (graph source destinations)
    (let ((best-distances (make-hash-table :test 'equal))
          (paths (make-hash-table :test 'equal)))
      (when destinations
        (flet ((coords (node) (first node))
               (distance (node) (second node))
               (path (node) (third node))
               (best-distance (coords) (gethash coords best-distances 33333)) ;; 33333 is more than the max distance in this problem
               (set-distance (coords dist) (setf (gethash coords best-distances) dist))
               (clear-paths (coords) (remhash coords paths))
               (add-path (coords path) (setf (gethash coords paths)
                                             (append (gethash coords paths nil)
                                                     (list path)))))
          (loop
             with queue = (queue-enqueue (make-queue) (list source 0 (list source)))
             until (queue-empty-p queue)
             for cur = (queue-dequeue queue) then (queue-dequeue queue)
             do (loop for n in (passable-neighbours graph (coords cur))
                   when (< (1+ (distance cur)) (best-distance n))
                   do (progn
                        (set-distance n (1+ (distance cur)))
                        (clear-paths n)
                        (queue-enqueue queue
                                       (list n
                                             (1+ (distance cur))
                                             (append (path cur) (list n))))))

             when (find (coords cur) destinations :test 'equal)
             do (progn
                  (add-path (coords cur) (path cur))
                  (return paths)))))
      paths)))

(defun filter-by-shortest (paths)
  (labels ((shortest-path-length ()
             (loop for key being the hash-keys in paths
                for value being the hash-values in paths
                minimize (length (first value))))

           (coordinates-of-shortest-paths ()
             (loop
                with shortest = (shortest-path-length)
                for key being the hash-keys in paths
                for value being the hash-values in paths
                when (= (length (first value)) shortest)
                collect key)))
    (loop
       for destination in (coordinates-of-shortest-paths)
       nconc (gethash destination paths))))

(defstruct unit
  (coords nil)
  (race nil)
  (hp nil))

(defun unit-p (graph x y)
  (find (get-square graph x y) "GE"))

(defun find-units (graph)
  (loop for x from 0 below (graph-width graph)
     nconc (loop for y from 0 below (graph-height graph)
              when (unit-p graph x y)
              collect (make-unit :coords (list x y) :race (get-square graph x y) :hp 200))))


(defun sort-by-reading-order (sequence &key (key #'identity))
  (let ((sorted-by-x (sort sequence #'< :key (lambda (coord) (x (funcall key coord))))))
    (stable-sort sorted-by-x #'< :key (lambda (coord) (y (funcall key coord))))))

(with-grid-deltas dx dy dirs
  (defun unit-destination-p (unit graph x y)
    (and (passable-p graph (list x y))
         (loop for dir in (dirs)
            when (enemy-p unit graph (+ x (x dir)) (+ y (y dir)))
            collect dir))))

(defun unit-enemy (unit)
  (if (char= (unit-race unit) #\E)
      #\G
      #\E))

(defun enemy-p (unit graph x y)
  (and (in-graph-p graph (list x y))
       (char= (get-square graph x y) (unit-enemy unit))))

(defun unit-destinations (unit graph)
  (loop for x from 1 below (1- (graph-width graph))
     nconc (loop for y from 1 below (1- (graph-height graph))
              when (unit-destination-p unit graph x y)
              collect (list x y))))

(defun unit-destination (unit graph)
  (first (sort-by-reading-order
          (filter-by-shortest 
           (bfs graph (unit-coords unit) (unit-destinations unit graph)))
          :key (lambda (path) (car (last path))))))

(defun unit-move (unit graph)
  (unless (unit-in-combat-p unit graph)
    (let ((old-coords (unit-coords unit)))
      (let ((dest (unit-destination unit graph)))
        (when dest
          (setf (unit-coords unit) (second dest))
          (update-graph graph old-coords #\.)
          (update-graph graph (unit-coords unit) (unit-race unit)))))))

(defun unit-dead-p (unit)
  (< (unit-hp unit) 1))

(defun graph-remove-unit (graph unit)
  (update-graph graph (unit-coords unit) #\.))

(defun unit-attack-power (unit elf-power)
  (if (char= (unit-race unit) #\G)
      3
      elf-power))

(defun unit-attack (unit graph units elf-power)
  (when (unit-in-combat-p unit graph)
    (let ((target-unit (unit-select-target unit units)))
      (unless target-unit
        ;; (format t "marked in combat, but did not find target unit in ~a~%" units)
        (print-battlefield graph units)
        (error "found target but no matching unit"))
      (when target-unit
        (decf (unit-hp target-unit) (unit-attack-power unit elf-power))
        (when (unit-dead-p target-unit)
          ;; (format t "removed dead unit: ~a~%" target-unit)
          (graph-remove-unit graph target-unit))))))

(defun unit-turn (unit graph units elf-power)
  (unless (unit-dead-p unit)
    (unit-move unit graph)
    (unit-attack unit graph units elf-power)))

(defun unit-description (unit)
  (format nil "~a(~a)" (unit-race unit) (unit-hp unit)))

(with-grid-deltas dx dy dirs
  (defun unit-in-combat-p (unit graph)
    (flet ((new-coord (direction-offset accessor)
             (+ (funcall accessor (unit-coords unit)) (funcall accessor direction-offset))))
      (first (loop for direction-offset in (dirs)
                for enemy-x = (new-coord direction-offset #'x) then (new-coord direction-offset #'x)
                for enemy-y = (new-coord direction-offset #'y) then (new-coord direction-offset #'y)
                when (enemy-p unit graph enemy-x enemy-y)
                collect (list enemy-x enemy-y)))))

  (defun unit-select-target (unit units)
    "Select a target from the unit list"
    (let* ((coords (loop for dir from 0 below (length (dirs))
                      collect (delta-coords (unit-coords unit) dir)))
           (potential-targets
            (remove-if #'unit-dead-p
                       (remove-if (lambda (u) (char= (unit-race unit) (unit-race u)))
                                  (remove-if #'null (mapcar (lambda (coord) (find coord units :key #'unit-coords :test #'equal))
                                                            coords))))))
      (loop
         with least-hp = 250
         with best-target = nil
         for target in potential-targets
         when (< (unit-hp target) least-hp)
         do (setf least-hp (unit-hp target)
                  best-target target)
         finally (return best-target)))))

(defun battle-turn (graph units elf-power)
  (remove-if #'unit-dead-p
             (loop
                with sorted-units = (sort-by-reading-order units :key #'unit-coords)
                for unit in sorted-units
                do (if (and (not (unit-dead-p unit)) (battle-over-p sorted-units))
                       (return (values sorted-units 0))
                       (unit-turn unit graph sorted-units elf-power))
                ;; do (unit-turn unit graph sorted-units)
                finally (return (values sorted-units 1)))))

(defun remaining-unit-counts (units)
  (loop with unit-counts = (make-hash-table :test 'equal)
     for unit in units
     do (incf (gethash (unit-race unit) unit-counts 0))
     finally (return unit-counts)))

(defun battle-over-p (units)
  (= 1 (loop for key being the hash-keys in (remaining-unit-counts units)
          count key)))

(defun battle-score (turns units)
  (* turns (loop for unit in units sum (unit-hp unit))))

(defun battle (graph units &optional (elf-power 3))
  (loop
     ;; for turns = 0 then (1+ turns)
     for remaining-units = (list units 0) then (multiple-value-list (battle-turn graph (first remaining-units) elf-power))
     until (battle-over-p (first remaining-units))
     ;; do (print-battlefield graph (first remaining-units))
     sum (or (second remaining-units) 1) into turns
     finally (return (values (battle-score turns (first remaining-units)) turns graph (first remaining-units)))))

  

(defun print-paths (paths)
  (loop for key being the hash-keys in paths
     for value being the hash-values in paths
     do (progn
          (format t "~a:~%" key)
          (loop for path in value do (format t "  ~a~%" path)))))

(print-paths
 (bfs (parse-file "15-test.txt") (list 1 5) (list (list 2 4) (list 1 1) (list 2 1) (list 1 3))))


(defun build-line (line line-idx &optional marks)
  (with-output-to-string (marked-line)
    (loop for chr across line
       for idx from 0 below (length line)
       do (if (find (list idx line-idx) marks :test #'equal)
              (write-char #\* marked-line)
              (write-char chr marked-line)))))

(defun print-graph (graph &optional marks)
  (format t "~{~a~%~}~%" 
          (loop for y from 0 below (graph-height graph)
             collect (build-line (aref graph y) y marks))))


(defun join (list separator &key (key #'identity))
  (with-output-to-string (joined)
    (loop for sep = "" then separator
       for elt in list
       do (format joined "~a~a" sep (funcall key elt)))))

(defun build-battlefield-line (graph line-idx units)
  (concatenate 'string
               (aref graph line-idx)
               "   "
               (join (sort
                      (remove-if-not (lambda (unit) (= line-idx (y (unit-coords unit)))) units)
                      #'< :key (lambda (unit) (x (unit-coords unit))))
                     ", "
                     :key #'unit-description)))

(defun print-battlefield (graph units)
  (format t "~{~a~%~}~%"
          (loop for y from 0 below (graph-height graph)
             collect (build-battlefield-line graph y units))))

(format t "~a~%"
        (sort-by-reading-order
         (filter-by-shortest
          (bfs (parse-file "15-test.txt")
               (list 1 5) (list (list 2 4) (list 1 1) (list 2 1) (list 1 3))))
         :key (lambda (c) (first (last c)))))

(format t "~a~%"
        (print-graph (parse-file "15-test.txt") (list (list 1 1))))
;; (format t "~a~%" (parse-file "15-test-1.txt"))

(defun doit (file &optional (elf-power 3))
  (battle (parse-file file) (find-units (parse-file file)) elf-power))

(defun power-without-elf-dead (file)
  (flet ((count-elves (units) (count-if (lambda (u) (char= #\E (unit-race u))) (remove-if #'unit-dead-p units))))
    (loop
       with elf-count = (count-elves (find-units (parse-file file)))
       for pow from 4 upto 200

       for battle-result = (multiple-value-list (battle (parse-file file) (find-units (parse-file file)) pow))
       then (multiple-value-list (battle (parse-file file) (find-units (parse-file file)) pow))
	   ;; do (format t "~a~%" (fourth battle-result))
       until (= (count-elves (fourth battle-result)) elf-count)
       finally (return pow))))


(defun doit2 (file)
  (doit file (power-without-elf-dead file)))

(time (format t "~a~%" (doit "15.txt")))
(time (format t "~a~%" (doit2 "15.txt")))
