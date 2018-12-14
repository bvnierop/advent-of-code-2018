;; initial-game: (list 0)
;; adding a marble (this one is hard!)
(defstruct marble-game-node
  prev
  next
  value)

(defun initialize-game ()
  (let ((node (make-marble-game-node :prev nil :next nil :value 0)))
    (setf (marble-game-node-prev node) node
          (marble-game-node-next node) node)
    node))

(defun add-marble-to-game (current-node marble-value)
  (let* ((before (marble-game-node-next current-node))
         (after (marble-game-node-next before))
         (new-node (make-marble-game-node :prev before :next after :value marble-value)))
    (setf (marble-game-node-next before) new-node
          (marble-game-node-prev after) new-node)
    new-node))

(defun remove-marble-from-game (current-node)
  (let ((before (marble-game-node-prev current-node))
        (after (marble-game-node-next current-node)))
    (setf (marble-game-node-next before) after
          (marble-game-node-prev after) before)
    after))

(defun print-game (first-node current-node)
  (declare (ignore current-node))
  (format t "~a~%"
          (with-output-to-string (S)
            (loop
               with circled = nil
               for cur = first-node then (marble-game-node-next cur)
               until (and circled (eq cur first-node))
               do (progn
                    (setf circled t)
                    (if (eq cur current-node)
                        (format S "(~a) " (marble-game-node-value cur))
                        (format S " ~a  " (marble-game-node-value cur))))))))

(defun special-marble-p (marble-value)
  (zerop (rem marble-value 23)))

(defun normal-move (current-marble next-marble-value)
  (add-marble-to-game current-marble next-marble-value))

(defun scoring-move (current-marble next-marble-value)
  (loop
     for _ from 0 below 7
     for cur = (marble-game-node-prev current-marble) then (marble-game-node-prev cur)
     finally (return (values 
                      (remove-marble-from-game cur)
                      (+ next-marble-value (marble-game-node-value cur))))))

(defun make-move (current-marble next-marble-value players player-count)
  (if (special-marble-p next-marble-value)
      (multiple-value-bind (new-current-marble score) (scoring-move current-marble next-marble-value)
        (incf (aref players (rem (1- next-marble-value) player-count)) score)
        new-current-marble)
      (normal-move current-marble next-marble-value)))

(defun play-game (game highest-marble player-count)
  (let ((players (apply #'vector (loop for _ from 0 below player-count collect 0))))
    (loop
       for i from 1 upto highest-marble
       for cur = (make-move game i players player-count) then (make-move cur i players player-count)
       finally (return (values cur players)))))

(defun high-score (players player-count)
  (loop for i from 0 below player-count
     maximizing (aref players i)))

(defun solve (highest-marble player-count)
  (let ((game (initialize-game)))
    (multiple-value-bind (current-marble players) (play-game game highest-marble player-count)
      ;; (print-game game current-marble)
      (format t "scores: ~a~%" players)
      (format t "high score: ~a~%" (high-score players player-count)))))

(solve 25 9)
(solve 1618 10)
(solve 7999 13)
(solve 1104 17)
(solve 6111 21)
(solve 5807 30)

(time (with-open-file (in "09.txt")
        (let ((player-count (read in))
              (junk (loop for _ from 0 below (length "players; last marble is worth") do (read-char in)))
              (highest-marble (read in)))
          (declare (ignore junk))
          (format t "input: ~a ~a~%" player-count highest-marble)
          (solve highest-marble player-count))))

(time (with-open-file (in "09.txt")
        (let ((player-count (read in))
              (junk (loop for _ from 0 below (length "players; last marble is worth") do (read-char in)))
              (highest-marble (read in)))
          (declare (ignore junk))
          (format t "input: ~a ~a~%" player-count highest-marble)
          (solve (* 100 highest-marble) player-count))))
