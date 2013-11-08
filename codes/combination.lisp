;;;;
(defun test-combination (n m)
  (let ((lst NIL))
    (dotimes (i n)
      (push (1+ i) lst))
    (combination-r lst m NIL)))

(defun combination-r (lst m prefix)
  (cond 
    ((<= m 1)
     (loop for x in lst
        collecting (append prefix (cons x NIL))))
    ((= (length lst) m)
     (cons (append prefix lst) NIL))
    (t
     (append (combination-r (cdr lst) (1- m) (append prefix (cons (car lst) NIL)))
             (combination-r (cdr lst) m prefix)))))
