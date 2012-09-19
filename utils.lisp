; Utility functions

(in-package #:tic-tac-toe)

(export '(max-by-key
          arow
          acol
          matrix-with-insertion))

(defun max-by-key (key arg &rest more-args)
  "Max arg as evaluated by key."
  (labels ((iter (args key-max arg-max)
             (if (null args)
                 arg-max
                 (let* ((x (car args))
                        (xs (cdr args))
                        (x-key (funcall key x)))
                   (if (> x-key key-max)
                       (iter xs x-key x)
                       (iter xs key-max arg-max))))))
    (iter more-args (funcall key arg) arg)))

(defun arow (array row)
  "The given row of the array."
  (let ((row-length (array-dimension array 1)))
    (loop for col below row-length collect (aref array row col))))

(defun acol (array col)
  "The given column of the array."
  (let ((col-length (array-dimension array 0)))
    (loop for row below col-length collect (aref array row col))))

(defun matrix-with-insertion (array i j x)
  "2d array with x inserted at i, j."
  (let ((array2 (alexandria:copy-array array)))
    (setf (aref array2 i j) x)
    array2))

(defun matrix-diagonal-tlbr (array)
  "Elements in the top-left to bottom-right diagonal of the given matrix"
  (let ((size (array-dimension array 0))) ; height == width
    (loop for i below size collect (aref array i i))))

(defun matrix-diagonal-bltr (array)
  "Elements in the bottom-left to top-right diagonal of the given matrix"
  (let ((size (array-dimension array 0))) ; height == width
    (loop
       for i below size
       for j from (- size 1) downto 0
       collect (aref array i j))))

(defun matrix-intercepts-tlbr (matrix-size i j)
  "Whether the given row and column intercept the top-left to bottom-right
diagonal in a matrix of the given size."
  (and (= i j)
       (< i matrix-size)))

(defun matrix-intercepts-bltr (matrix-size i j)
  "Whether the given row and column intercept the bottom-left to top-right
diagonal in a matrix of the given size."
  (= (- matrix-size i 1) j))

(defun equals-symbol (symbol)
  "Returns a predicate for checking whether a symbol is the given symbol."
  (lambda (x)
    (eq x symbol)))
