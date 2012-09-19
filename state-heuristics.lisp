; State scoring heuristics

(in-package #:tic-tac-toe)

; Simple heuristic - calculates how many lines that the player can win on are
; left, and how full they are.

(defun score-state (state player)
  (let ((board (game-state-board state))
        (enemy (next-player player))
        (base-score 2)           ; base score for an available line
        (occupation-score 1))    ; extra score per piece in line
    (labels ((contains-enemy (line)
               (some (lambda (pos)
                       (eq pos enemy)) line))
             (score-line (line)
               (+ base-score
                  (* occupation-score (count-if (equals-symbol player) line)))))
      (apply #'+ 0
             (mapcar #'score-line
                     (remove-if #'contains-enemy
                                (lines board)))))))
