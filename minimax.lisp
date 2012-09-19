; Minimax scoring algorithm

(in-package #:tic-tac-toe)

(defconstant +minimax-depth+ 3)
(defconstant +end-game-score+ 999999)

(defmethod best-move ((state game-state))
  (apply #'max-by-key (lambda (move)
                        (- (minimax (after-move state move) #'score-state +minimax-depth+)))
         (state-available-moves state)))

(defun naive-minimax (state)
  "Naive version of the Minimax algorithm - explores the whole state tree,
which is infeasibly complicated for anything larger than a 3x3 board."
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

(defun minimax (state heuristic max-depth)
  (let* ((player (game-state-whose-turn state))
         (enemy (next-player player)))
    (labels ((iter (state depth)
               (let ((next-states (branches state)))
                 (cond ((null next-states) 0)
                       ((wins? player state) +end-game-score+)
                       ((wins? enemy state) (- +end-game-score+))
                       ((= depth max-depth) (funcall heuristic state player))
                       (t (apply (if (eq (game-state-whose-turn state) player)
                                     #'max #'min)
                                 (mapcar (lambda (state)
                                           (iter state (1+ depth)))
                                         next-states)))))))
      (iter state 0))))
