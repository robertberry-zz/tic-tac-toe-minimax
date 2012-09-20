; State representation

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

(defgeneric best-move (state)
  (:documentation "The best move to take in the given state."))

(defgeneric branches (state)
  (:documentation "Other possible states after available moves in this state."))

(defgeneric sorted-branches (state)
  (:documentation "Child states sorted by how decent the move is predicted to be."))

(defmethod print-object ((state game-state) stream)
  (print-object (game-state-board state) stream)
  (format stream "~a's turn~%" (game-state-whose-turn state)))

(defmethod best-move ((state game-state))
  (apply #'max-by-key (lambda (move)
                        (- (alpha-beta
                            (after-move state move)
                            #'score-state
                            #'sorted-branches
                            +minimax-depth+)))
         (state-available-moves state)))

(defmethod after-move ((state game-state) move)
  (let ((player (game-state-whose-turn state)))
    (make-instance 'game-state
                   :turn (next-player player)
                   :board (after-move (game-state-board state) move))))

(defmethod branches ((state game-state))
  (mapcar (lambda (move)
            (after-move state move))
          (state-available-moves state)))

(defmethod sorted-branches ((state game-state))
  (mapcar (lambda (move)
            (after-move state move))
          (sort (state-available-moves state) #'>
                :key (lambda (move)
                       (move-score-improved (game-state-board state) move)))))

(defun state-available-moves (state)
  "Available moves for the player whose turn it is in the given state."
  (let ((board (game-state-board state))
        (player (game-state-whose-turn state)))
    (available-moves board player)))

(defun wins? (player state)
  "Whether the player wins in the given state."
  (some (lambda (line)
            (every (lambda (square)
                     (eq square player)) line))
        (lines (game-state-board state))))

(defun draw? (state)
  "Whether the players draw in the given state."
  (let ((board (game-state-board state)))
    (not (some (lambda (pos)
                 (position-empty? board (car pos) (cadr pos))) (positions board)))))
