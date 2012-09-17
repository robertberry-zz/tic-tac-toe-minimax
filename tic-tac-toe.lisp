; Tic-tac-toe

(in-package #:tic-tac-toe)

(export '(game-loop))

(defun get-player-move (piece)
  (format t "Move? ")
  (let ((move (read)))
    (make-instance 'game-move
                   :column (car move)
                   :row (cadr move)
                   :piece piece)))

(defun get-cpu-move (state)
  (best-move state))

(defun game-loop (player size)
  (let ((cpu (next-player player)))
    (labels ((iter (state)
               (print state)
               (cond ((wins? player state) (format t "You win!"))
                     ((wins? cpu state) (format t "You lose!"))
                     ((draw? state) (format t "A draw!"))
                     (t (let* ((whose-turn (game-state-whose-turn state))
                               (move (if (eq whose-turn player)
                                         (get-player-move player)
                                         (get-cpu-move state))))
                          (iter (after-move state move)))))))
      (iter (make-instance 'game-state
                           :turn 'X
                           :board (make-instance 'game-board
                                                 :size size))))))
