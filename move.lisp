; Move representation

(in-package #:tic-tac-toe)

(defclass game-move ()
  ((piece :initarg :piece
          :accessor move-piece
          :documentation "Piece making the move.")
   (row :initarg :row
        :accessor move-row
        :documentation "Row to place the piece.")
   (column :initarg :column
           :accessor move-column
           :documentation "Column to place the piece.")))

(defmethod print-object ((move game-move) stream)
  (format stream
          "<game-move ~a to (~a, ~a)>"
          (move-piece move) (move-row move) (move-column move)))
