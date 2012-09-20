; Minimax scoring algorithms

(in-package #:tic-tac-toe)

(defconstant +minimax-depth+ 3
  "Maximum search depth for minimax algorithm.")

(defconstant +end-game-score+ 999999
  "Very large score for winning game (should be higher than any score returned
by a heuristic.")

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
  "Full minimax. Given a game state, a heuristic function, and a maximum
depth, calculates the score."
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

(defconstant +very-low-number+ -999999
  "Large negative number (absolute value should be larger than any score
  returned by a heuristic.")

(defconstant +very-high-number+ 999999
  "Large number (value should be larger than any score returned by a
  heuristic.")

(defun alpha-beta (state heuristic children max-depth)
  "Minimax with alphabeta pruning. Given a game state, a heuristic function,
a function for getting children of a state (preferably ordered by best-first),
and a maximum depth, calculates the score."
  (let* ((player (game-state-whose-turn state))
         (enemy (next-player player)))
    (labels ((maximize (states alpha beta depth)
               (if (null states)
                   alpha
                   (let* ((state (car states))
                          (alpha (max alpha (iter state (+ depth 1) alpha beta))))
                     (if (<= beta alpha)
                         alpha
                         (maximize (cdr states) alpha beta depth)))))
             (minimize (states alpha beta depth)
               (if (null states)
                   beta
                   (let* ((state (car states))
                          (beta (min alpha (iter state (+ depth 1) alpha beta))))
                     (if (<= beta alpha)
                         beta
                         (minimize (cdr states) alpha beta depth)))))
             (iter (state depth
                          &optional (alpha +very-low-number+) (beta +very-high-number+))
               (let ((next-states (funcall children state)))
                 (cond ((null next-states) 0)
                       ((wins? player state) +end-game-score+)
                       ((wins? enemy state) (- +end-game-score+))
                       ((= depth max-depth) (funcall heuristic state player))
                       ((eq (game-state-whose-turn state) player)
                        (maximize next-states alpha beta depth))
                       (t (minimize next-states alpha beta depth))))))
      (iter state 0))))

