; Board representation

(in-package #:tic-tac-toe)

(export '(rows
          columns
          diagonals
          lines
          valid-move?
          after-move
          available-moves
          position-empty?
          position-intercepts))

(defconstant +default-size+ 3)
(defconstant +empty-space+ '-)

(defclass game-board ()
  ((size :initarg :size
         :accessor game-board-size
         :initform +default-size+
         :documentation "Number of columns and rows in the game board.")
   (grid :accessor game-board-grid
         :initarg :grid
         :initform nil
         :documentation "2-dimensional vector of the pieces on the board.")))

(defmethod initialize-instance :after ((board game-board) &key)
  (let ((grid (game-board-grid board)))
    (if (null grid)
        (let ((size (game-board-size board)))
          (setf (slot-value board 'grid)
                (make-array `(,size ,size) :initial-element +empty-space+))))))

(defmethod print-object ((board game-board) stream)
  (let ((size (game-board-size board))
        (grid (game-board-grid board)))
    (flet ((print-row-divisor ()
             (loop for i below size do
                  (format stream "|---")
                finally (format stream "|~%"))))
      (loop for i below size
         initially (print-row-divisor)
         do (loop for j below size
               do (format stream "| ~a " (aref grid i j))
               finally (prog nil
                          (format stream "|~%")
                          (print-row-divisor)))))))

(defgeneric rows (board)
  (:documentation "List of rows in board grid."))

(defgeneric columns (board)
  (:documentation "List of columns in board grid."))

(defgeneric diagonals (board)
  (:documentation "List of diagonals in board grid."))

(defgeneric lines (board)
  (:documentation "List of rows, columns, and diagonals in board grid."))

(defgeneric valid-move? (board move)
  (:documentation "Whether move is possible on board."))

(defgeneric after-move (board move)
  (:documentation "The board after the given move."))

(defgeneric available-moves (board piece)
  (:documentation "Available moves on board for piece."))

(defgeneric position-empty? (board row column)
  (:documentation "Whether the given position on the board is empty."))

;; a heuristic to find how many lines a given position intercepts
(defgeneric position-intercepts (board row column)
  (:documentation "How many lines the given position intercepts."))

;; possibly a better heuristic would be to look at those lines in more detail

(defgeneric move-score (board move)
  (:documentation "A score heuristic for a given move on a given board."))

(defgeneric move-score-improved (board move)
  (:documentation "An improved score heuristic for a given move on a given board."))

(defgeneric score-empty-squares (board heuristic piece)
  (:documentation "Given a board and a scoring heuristic, scores the empty
  squares for the given piece."))

(defgeneric position-lines (board row col)
  (:documentation "The lines the row and col on the board intercepts."))

(defgeneric board-position (board row column)
  (:documentation "The piece in the given position on the board."))

(defgeneric positions (board)
  (:documentation "A list of row, column positions on the board."))

(defmethod positions ((board game-board))
  (let ((size (game-board-size board)))
    (apply #'append
           (loop for i below size
                collect (loop for j below size
                             collect (list i j))))))

(defmethod rows ((board game-board))
  (let ((n-rows (game-board-size board))
        (grid (game-board-grid board)))
    (loop for row below n-rows collect (arow grid row))))

(defmethod columns ((board game-board))
  (let ((n-cols (game-board-size board))
        (grid (game-board-grid board)))
    (loop for col below n-cols collect (acol grid col))))

(defmethod diagonals ((board game-board))
  (let ((grid (game-board-grid board)))
    (list
     (matrix-diagonal-tlbr grid)
     (matrix-diagonal-bltr grid))))

(defmethod lines ((board game-board))
  (append (rows board)
          (columns board)
          (diagonals board)))

(defmethod board-position ((board game-board) row column)
  (let ((grid (game-board-grid board)))
    (aref grid row column)))

(defmethod position-empty? ((board game-board) row column)
  (eq (board-position board row column) +empty-space+))

(defmethod valid-move? ((board game-board) (move game-move))
  (let ((i (move-row move))
        (j (move-column move)))
    (position-empty? board i j)))

(defmethod after-move ((board game-board) (move game-move))
  (let ((size (game-board-size board))
        (grid (game-board-grid board))
        (i (move-row move))
        (j (move-column move))
        (piece (move-piece move)))
    (if (valid-move? board move)
        (make-instance 'game-board
                       :size size
                       :grid (matrix-with-insertion grid i j piece)))))

(defmethod available-moves ((board game-board) piece)
  (let ((size (game-board-size board)))
    (apply #'append (loop for i below size
                       collect (loop for j below size
                                  when (position-empty? board i j)
                                  collect (make-instance 'game-move
                                                         :row i
                                                         :column j
                                                         :piece piece))))))

(defmethod position-intercepts ((board game-board) row column)
  (let ((intercepts-diagonal-1 (= row column))
        (intercepts-diagonal-2 (= (- (game-board-size board) row 1) column)))
    (+ 2    ; always intercepts a row and column
       (if intercepts-diagonal-1 1 0)
       (if intercepts-diagonal-2 1 0))))

(defmethod position-lines ((board game-board) row column)
  (let* ((grid (game-board-grid board))
         (size (game-board-size board))
         (row-line (arow grid row))
         (column-line (acol grid column))
         (diag1 (if (matrix-intercepts-tlbr size row column)
                    (matrix-diagonal-tlbr grid)
                    nil))
         (diag2 (if (matrix-intercepts-bltr size row column)
                    (matrix-diagonal-bltr grid)
                    nil)))
    (remove-if #'null (list row-line column-line diag1 diag2))))

; A heuristic function for the score of a given move. Finds any lines that
; have not been blocked by enemy pieces and scores them. They are scored based
; on how many of the positions are already occupied by the player's pieces.

(defmethod move-score ((board game-board) (move game-move))
  (let ((row (move-row move))
        (col (move-column move))
        (player (move-piece move))
        (player-occupied-score 2)
        (not-occupied-score 1))    
    (labels ((score-line (line &optional (acc 0))
               (if (null line)
                   acc
                   (let ((first (car line))
                         (rest (cdr line)))
                     (cond ((eq first player) (score-line rest (+ acc
                                                                  player-occupied-score)))
                           ((eq first +empty-space+) (score-line rest (+ acc
                                                                         not-occupied-score)))
                           (t 0)))))) ; any line with an enemy position scores 0
      (apply #'+ (mapcar #'score-line (position-lines board row col))))))

;; Thinking about it this heuristic is not very good as it does not take into
;; account blocking the enemy. What it should do is score 0 for lines that
;; contain both player and enemy pieces, but score positively for lines that
;; only contain player or only contain enemy pieces.

(defun mixed-line (line)
  "Whether the line contains pieces for both players."
  (labels ((equals-symbol (symbol)
             (lambda (x)
               (eq x symbol)))
           (contains-O (line)
             (some (equals-symbol 'O) line))
           (contains-X (line)
             (some (equals-symbol 'X) line))
           (iter (line)
             (if (null line)
                 nil
                 (let ((first (car line)))
                   (cond ((eq first 'X) (contains-O line))
                         ((eq first 'O) (contains-X line))
                         (t (iter (cdr line))))))))
    (iter line)))

(defmethod move-score-improved ((board game-board) (move game-move))
  (let ((row (move-row move))
        (col (move-column move))
        (base-score 1)     ; base score given for a non-mixed line
        (piece-score 1))   ; extra score given per occupied position in a non-mixed line
    (labels ((not-empty (position)
               (not (eq position +empty-space+)))
             (score-line (line)
               (+ base-score
                  (* piece-score (count-if #'not-empty line)))))
      (apply #'+
             (mapcar #'score-line
                     (remove-if #'mixed-line
                                (position-lines board row col)))))))

(defmethod score-empty-squares ((board game-board) heuristic piece)
  (let ((size (game-board-size board))
        (score-grid (alexandria:copy-array (game-board-grid board))))
    (loop for i below size do
         (loop for j below size do
              (if (position-empty? board i j)
                  (setf (aref score-grid i j) (funcall heuristic board
                                                       (make-instance 'game-move
                                                                      :piece piece
                                                                      :row i
                                                                      :column j)))
                  (setf (aref score-grid i j) (board-position board i j)))))
    ; Kind of hacky: puts it into a game-board object so we can use the same fancy
    ; print function. todo: refactor
    (make-instance 'game-board :size size :grid score-grid))) 
