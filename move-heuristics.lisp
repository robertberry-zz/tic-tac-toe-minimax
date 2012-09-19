; Heuristic scoring functions for moves. This is used to order which moves to
; check in alpha-beta pruning.

(in-package #:tic-tac-toe)

(export '(move-score
          move-score-improved
          score-empty-squares
          position-lines))

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
  (labels ((contains-O (line)
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
