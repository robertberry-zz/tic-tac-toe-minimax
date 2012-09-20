; Board representation (as multi-dimensional array)

(in-package #:tic-tac-toe)

(export '(rows
          columns
          diagonals
          lines
          valid-move?
          after-move
          available-moves
          position-empty?))

(defconstant +default-size+ 3
  "Default size of a board.")

(defconstant +empty-space+ '-
  "Symbol used to denote an empty space on the board.")

(defclass game-board ()
  ((size :initarg :size
         :accessor game-board-size
         :initform +default-size+
         :documentation "Number of columns and rows in the game board.")
   (grid :accessor game-board-grid
         :initarg :grid
         :initform nil
         :documentation "2-dimensional vector of the pieces on the board.")))

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

(defgeneric board-position (board row column)
  (:documentation "The piece in the given position on the board."))

(defgeneric positions (board)
  (:documentation "A list of row, column positions on the board."))

(defmethod initialize-instance :after ((board game-board) &key)
  (let ((grid (game-board-grid board)))
    (if (null grid)
        (let ((size (game-board-size board)))
          (setf (slot-value board 'grid)
                (make-array `(,size ,size) :initial-element +empty-space+))))))

(defmethod print-object ((board game-board) stream)
  (let ((size (game-board-size board))
        (grid (game-board-grid board)))
    (flet ((print-row-names ()
             (format stream "   ")
             (mapcar (lambda (ch)
                       (format stream "  ~a " ch)) (letters size))
             (format stream "~%"))
           (print-row-divisor ()
             (loop for i below size
                  initially (format stream "   ")
                do
                  (format stream "|---")
                finally (format stream "|~%"))))
      (loop for i below size
         initially (prog nil
                      (print-row-names)
                      (print-row-divisor))
         do (loop for j below size
                 initially (format stream " ~a " (1+ i))
               do (format stream "| ~a " (aref grid i j))
               finally (prog nil
                          (format stream "|~%")
                          (print-row-divisor)))))))

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

