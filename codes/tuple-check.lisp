


(defun tuple-check (lst)
  (if (equal (first lst) #\()
      (multiple-value-bind (result rest)
          (tuple-check-rest (rest lst))
        (and result (null rest)))
      NIL))
  
(defun tuple-check-rest (lst)
  (if (equal (first lst) #\))
      (values T (rest lst))
      (tuple-check-content lst)))

(defun tuple-check-content (lst)
  (let ((tuple-check-content-next (lambda (lst)
                                     (cond
                                       ((equal (first lst) #\,)
                                        (tuple-check-content (rest lst)))
                                       ((equal (first lst) #\))
                                        (values t (rest lst)))
                                       (t NIL)))))
  (cond 
    ((equal (first lst) #\() 
     (multiple-value-bind (result rest)
         (tuple-check-rest (rest lst))
       (if result
           (funcall tuple-check-content-next rest)
           (values result NIL))))
    ((digit-char-p (first lst))
     (multiple-value-bind (result rest)
         (tuple-check-int (rest lst))
       (funcall tuple-check-content-next rest)))
    (t NIL))))

(defun tuple-check-int (lst)
  (if (and lst (digit-char-p (first lst)))
      (tuple-check-int (rest lst))
      (values t lst)))



(defun tuple-check-test ()
  (loop
     (let ((s (read-line)))
       (when (string-equal s "q")
         (return))
       (format t "~A~%" (tuple-check 
                         (loop for i from 0 to (1- (length s))
                            collecting (aref s i)))))))
