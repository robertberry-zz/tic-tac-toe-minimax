;; Tic-tac-toe

(load "utils.lisp")
(load "move.lisp")
(load "board.lisp")

(defclass game-state ()
  ((board :initarg :board
          :accessor game-state-board
          :documentation "The current board")
   (whose-turn :initarg :turn
               :accessor game-state-whose-turn
               :initform 'X
               :documentation "Which player's turn it currently is.")))

(defmethod print-object ((state game-state) stream)
  (print-object (game-state-board state) stream)
  (format stream "~a's turn~%" (game-state-whose-turn state)))

(defgeneric best-move (state))

(defgeneric branches (state))

(defmethod branches ((state game-state))
  (mapcar (lambda (move)
            (after-move state move))
          (state-available-moves state)))

(defun state-available-moves (state)
  (let ((board (game-state-board state))
        (player (game-state-whose-turn state)))
    (available-moves board player)))

(defmethod best-move ((state game-state))
  "The best move for a player to take in a given state."
  (apply #'max-by-key (lambda (move)
                        (- (minimax (after-move state move))))
         (state-available-moves state)))

(defun wins? (player state)
  (some (lambda (line)
            (every (lambda (square)
                     (eq square player)) line))
        (lines (game-state-board state))))

(defun draw? (state)
  (let ((board (game-state-board state)))
    (not (some (lambda (pos)
                 (position-empty? board (car pos) (cadr pos))) (positions board)))))

(defun minimax (state)
  (let* ((player (game-state-whose-turn state))
         (enemy (next-player player)))
    (labels ((iter (state)
               (let ((next-states (branches state)))
                 (cond ((null next-states) 0)
                       ((wins? player state) 1)
                       ((wins? enemy state) -1)
                       (t (apply (if (eq (game-state-whose-turn state) player)
                                     #'max #'min)
                                 (mapcar #'iter next-states)))))))
      (iter state))))

(defun get-player-move (state piece)
  (format t "Move? ")
  (let ((move (read)))
    (make-instance 'game-move :column (car move) :row (cadr move) :piece piece)))

(defun get-cpu-move (state)
  (best-move state))

(defun next-player (player)
  "Next player after player."
  (if (eq player 'X) 'O 'X))

(defmethod after-move ((state game-state) move)
  (let ((player (game-state-whose-turn state)))
    (make-instance 'game-state
                   :turn (next-player player)
                   :board (after-move (game-state-board state) move))))

(defun game-loop (player size)
  (let ((cpu (next-player player)))
    (labels ((iter (state)
               (print state)
               (cond ((wins? player state) (format t "You win!"))
                     ((wins? cpu state) (format t "You lose!"))
                     ((draw? state) (format t "A draw!"))
                     (t (let* ((whose-turn (game-state-whose-turn state))
                               (move (if (eq whose-turn player)
                                         (get-player-move state player)
                                         (get-cpu-move state))))
                          (iter (after-move state move)))))))
      (iter (make-instance 'game-state
                           :turn 'X
                           :board (make-instance 'game-board
                                                 :size size))))))
