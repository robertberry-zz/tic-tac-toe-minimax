(in-package #:tic-tac-toe)

(export '(branches
          best-move
          wins?
          draw?
          next-player))

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

(defgeneric best-move (state)
  (:documentation "The best move to take in the given state."))

(defgeneric branches (state)
  (:documentation "Other possible states after available moves in this state."))

(defmethod after-move ((state game-state) move)
  (let ((player (game-state-whose-turn state)))
    (make-instance 'game-state
                   :turn (next-player player)
                   :board (after-move (game-state-board state) move))))

(defmethod branches ((state game-state))
  (mapcar (lambda (move)
            (after-move state move))
          (state-available-moves state)))

(defun state-available-moves (state)
  (let ((board (game-state-board state))
        (player (game-state-whose-turn state)))
    (available-moves board player)))

(defmethod best-move ((state game-state))
  (apply #'max-by-key (lambda (move)
                        (- (minimax (after-move state move))))
         (state-available-moves state)))

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

(defun wins? (player state)
  (some (lambda (line)
            (every (lambda (square)
                     (eq square player)) line))
        (lines (game-state-board state))))

(defun draw? (state)
  (let ((board (game-state-board state)))
    (not (some (lambda (pos)
                 (position-empty? board (car pos) (cadr pos))) (positions board)))))

(defun next-player (player)
  "Next player after player."
  (if (eq player 'X) 'O 'X))
