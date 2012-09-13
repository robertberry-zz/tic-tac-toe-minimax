;; Tic-tac-toe

(load "utils.lisp")
(load "board.lisp")

(defclass game-state ()
  ((board :initarg :board
          :accessor game-state-board
          :initform *initial-game-board*
          :documentation "The current board")
   (whose-turn :initarg :turn
               :accessor game-state-whose-turn
               :initform 'X
               :documentation "Which player's turn it currently is.")))

(defgeneric branches (state))

(defmethod branches ((state game-state))
  (let* ((player (game-state-whose-turn state))
         (next (next-player player)))
    (mapcar (lambda (board)
              (make-instance 'game-state :board board :turn next))
            (moves (game-state-board state) player))))

(defmethod print-object ((state game-state) stream)
  (print-board (game-state-board state) stream)
  (format stream "~a's turn~%" (game-state-whose-turn state)))

(defparameter *start-state* (make-instance 'game-state))

(defgeneric best-move (state))

(defmethod best-move ((state game-state))
  "The best move for a player to take in a given state."
  (apply #'max-by-key (lambda (state)
                        (- (minimax state))) (branches state)))

(defun wins? (player state)
  (some (lambda (line)
            (every (lambda (square)
                     (eq square player)) line))
        (lines (game-state-board state))))

(defun draw? (state)
  (every (lambda (square)
           (not (eq square '-))) (game-state-board state)))

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

(defun get-player-move (state)
  (format t "Move? ")
  (read))

(defun get-cpu-move (state)
  (let ((move (best-move state)))
    (diff-boards (game-state-board state) (game-state-board move))))

(defun apply-move (state move)
  (let ((player (game-state-whose-turn state)))
    (make-instance 'game-state :turn (next-player player)
                   :board (board-with-placement (game-state-board state) player move))))

(defun game-loop ()
  (let ((player 'X)
        (cpu 'O))
    (labels ((iter (state)
               (print state)
               (cond ((wins? player state) (format t "You win!"))
                     ((wins? cpu state) (format t "You lose!"))
                     ((draw? state) (format t "A draw!"))
                     (t (let* ((whose-turn (game-state-whose-turn state))
                               (move (if (eq whose-turn player)
                                         (get-player-move state)
                                         (get-cpu-move state))))
                          (iter (apply-move state move)))))))
      (iter *start-state*))))

