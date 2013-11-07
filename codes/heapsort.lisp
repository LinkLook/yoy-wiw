;;;;
(defun exchange (array i j)
  (let ((tmp (aref array i)))
    (setf (aref array i) (aref array j))
    (setf (aref array j) tmp))
  array)
;;Recursively exchange value between parent and children when either child is bigger.
(defun heapsort-maxup (array i heap-size)
  (let* ((left (1+ (* 2 i)))
         (right (* 2 (1+ i)))
         (bigger (if (< right heap-size)
                     (if (< (aref array left) (aref array right)) right left)
                     (if (< left heap-size) left i))))
    (when (< (aref array i) (aref array bigger))
      (heapsort-maxup (exchange array i bigger) bigger heap-size))
    array))
;;Make the array heap structrue.
(defun heapsort-build (array heap-size)
  (let ((half (floor (/ heap-size 2))))
    (loop until (< half 1)
       do (setf array (heapsort-maxup array (decf half) heap-size)))
    array))
;;Heapsort funtion.
(defun heapsort (array)
  (loop for heap-size from (length array) downto 1
     do (setf array (exchange (heapsort-build array heap-size) 0 (1- heap-size))))
  array)
;;For testing.
(defun heapsort-test (count)
  (let ((array (make-array count :element-type 'integer)))
    (dotimes (i count)
      (setf (aref array i) (random (* count 10))))
    (format t "~A~%" array)
    (format t "~A~%" (heapsort array))))
       
