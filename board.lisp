;; Board representation

(defparameter +rows+ 3)
(defparameter +columns+ 3)

(defparameter *initial-game-board*
  '(- - -
    - - -
    - - -))

(defun print-board (board &optional (stream t))
  "Prints the game board."
  (prog nil
     (mapcar (lambda (row)
               (format stream "~{~a ~}~%" row))
             (n-sized-chunks 3 board))))

(defun occupied-X? (space)
  "Whether given space is occupied by X."
  (eq space 'X))

(defun occupied-O? (space)
  "Whether given space is occupied by O."
  (eq space 'O))

(defun empty-space? (space)
  "Whether given space is empty."
  (eq space '-))

(defun board-with-placement (board piece n)
  "board with piece placed at the nth position."
  (append (take n board) (list piece) (drop (1+ n) board)))

(defun moves (board piece)
  "All possible moves for the given piece on the given board (assuming it is
the piece's turn)."
  (let ((bsize (length board)))
    (labels ((iter (n)
               (if (= n bsize)
                   '()
                   (if (empty-space? (nth n board))
                       (cons (board-with-placement board piece n)
                             (iter (1+ n)))
                       (iter (1+ n))))))
      (iter 0))))

(defun X-moves (board)
  "All possible moves for x on the given board (assuming it is x's turn)."
  (moves board 'X))

(defun O-moves (board)
  "All possible moves for y on the given board (assuming it is y's turn)."
  (moves board 'O))

(defun rows (board)
  "List of rows in board."
  (n-sized-chunks +rows+ board))

(defun columns (board)
  "List of columns in board."
  (apply #'mapcar #'list (rows board)))

(defun diagonals (board)
  "List of diagonals in board."
  ; just hardcode this for now
  (list (nths '(0 4 8) board)
        (nths '(2 4 6) board)))

(defun lines (board)
  "All the possible lines to win on on the board."
  (append (rows board)
          (columns board)
          (diagonals board)))

(defun next-player (player)
  "Next player after player."
  (if (eq player 'X) 'O 'X))

(defun diff-boards (a b)
  "Index of first difference in a and b."
  (labels ((iter (a b n)
             (if (null a)
                 nil
                 (if (not (eq (car a) (car b)))
                     n
                     (iter (cdr a) (cdr b) (1+ n))))))
    (iter a b 0)))

