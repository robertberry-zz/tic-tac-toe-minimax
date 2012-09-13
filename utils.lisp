;; Utility functions

(defun take (n seq)
  "Returns the first n elements from seq."
  (if (= n 0)
      '()
      (cons (car seq) (take (- n 1) (cdr seq)))))

(defun drop (n seq)
  "Returns seq without the first n elements."
  (cond ((= n 0) seq)
        ((< (length seq) n) '())
        (t (drop (- n 1) (cdr seq)))))
            
(defun n-sized-chunks (n seq)
  "Returns seq separated into n-sized subsequences."
  (cond ((null seq) '())
        ((< (length seq) n)
         (list seq))
        (t (cons (take n seq) (n-sized-chunks n (drop n seq))))))

(defun every-nth (n seq)
  "Every nth element of seq."
  (labels ((iter (i seq)
             (if (null seq)
                 '()
                 (if (= i 1)
                     (cons (car seq) (iter n (cdr seq)))
                     (iter (- i 1) (cdr seq))))))
    (iter n seq)))

(defun nths (ns seq)
  "Extract elements at ns positions from seq. ns should be in ascending order."
  (labels ((iter (n ns seq)
             (if (or (null seq) (null ns))
                 '()
                 (let ((next-n (car ns)))
                   (if (= n next-n)
                       (cons (car seq) (iter (1+ n) (cdr ns) (cdr seq)))
                       (iter (1+ n) ns (cdr seq)))))))
    (iter 0 ns seq)))

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
