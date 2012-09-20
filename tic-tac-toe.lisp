; Tic-tac-toe

(in-package #:tic-tac-toe)

(export '(game-loop))

(defun get-player-move (piece board size)
  "Asks the player for a move."
  (labels ((get-coordinate ()
             (let ((gridref (read-line)))
               (let ((coord (gridref->coord gridref size)))
                 (cond ((null coord)
                        (format t "Please enter a valid co-ordinate: ")
                        (get-coordinate))
                       ((not (position-empty? board (cadr coord) (car coord)))
                        (format t "That position is occupied. Try again: ")
                        (get-coordinate))
                       (t coord))))))
    (format t "Enter a co-ordinate (e.g. 'c2'): ")
    (let ((move (get-coordinate)))
      (make-instance 'game-move
                     :column (car move)
                     :row (cadr move)
                     :piece piece))))

(defun gridref->coord (str board-size)
  "Converts a grid-reference, like 'a1', to a coordinate, e.g. 0, 0."
  (let ((columns (letters board-size))
        (rows (numbers board-size))
        (components (string->list str)))
    (if (/= (length components) 2)
        nil
        (let ((col (car components))
              (row (digit-char-p (cadr components))))
          (cond ((null row) nil)
                ((not (member row rows)) nil)
                ((not (member col columns)) nil)
                (t (list (letter->number col) (1- row))))))))

; Gets the next CPU move
(defun get-cpu-move (state)
  (best-move state))

(defun game-loop (&optional (player 'X) (size 3))
  "Start a game as the given player for the given size x size board."
  (let ((cpu (next-player player)))
    (labels ((iter (state)
               (print state)
               (cond ((wins? player state) (format t "You win!"))
                     ((wins? cpu state) (format t "You lose!"))
                     ((draw? state) (format t "A draw!"))
                     (t (let* ((whose-turn (game-state-whose-turn state))
                               (move (if (eq whose-turn player)
                                         (get-player-move player
                                                          (game-state-board state) size)
                                         (get-cpu-move state))))
                          (iter (after-move state move)))))))
      (iter (make-instance 'game-state
                           :turn 'X
                           :board (make-instance 'game-board
                                                 :size size))))))
