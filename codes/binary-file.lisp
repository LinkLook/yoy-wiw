

(defvar *in-progress-objects* nil)

(defgeneric read-object (obj stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill the slots of object from stream."))

(defgeneric write-object (obj stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write the slots of object to stream."))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of given type from stream."))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as the given type to stream."))

(defmethod read-object :around (obj stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons obj *in-progress-objects*)))
    (call-next-method)))

(defmethod write-object :around (obj stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons obj *in-progress-objects*)))
    (call-next-method)))

(defun current-binary-object ()
  (first *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms 
                      for n in names
                      collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names
                        for g in gensyms
                        collect `(,n ,g)))
                ,@body)))))

(defun mklist (x)
  (if (listp x) x (list x)))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun as-keyword (sym)
  (intern (string sym) :keyword))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defun slot->binding (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))

(defun direct-slots (name)
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  (loop for super in (get name 'superclasses)
       nconc (direct-slots super)
       nconc (inherited-slots super)))

(defun all-slots (name)
  (nconc (direct-slots name) (inherited-slots name)))

(defun new-class-all-slots (slots superclasses)
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))
    
(defmacro define-binary-type (name (&rest args) &body spec)
  (ecase (length spec)
    (1 (with-gensyms (type stream value)
         (destructuring-bind (derived-from &rest derived-args) (mklist (first spec))
           `(progn
              (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
                (read-value ',derived-from ,stream ,@derived-args))
              (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
                (write-value ',derived-from ,stream ,value ,@derived-args))))))
    (2 (with-gensyms (type)
         `(progn
            ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
                                 `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args) ,@body))
            ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
                                 `(defmethod write-value ((,type (eql ',name)) ,out ,value &key ,@args) ,@body)))))))


(defmacro define-generic-binary-class (name (&rest superclasses) slots read-method)
  (with-gensyms (objvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'slots) ',(mapcar #'first slots))
         (setf (get ',name 'superclasses) ',superclasses))
       (defclass ,name ,superclasses
         ,(mapcar #'slot->defclass-slot slots))
       ,read-method
       (defmethod write-object progn ((,objvar ,name) ,streamvar)
                  (declare (ignorable ,streamvar))
                  (with-slots ,(new-class-all-slots slots superclasses) ,objvar
                    ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

(defmacro define-binary-class (name (&rest superclasses) slots)
  (with-gensyms (objvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
                                  (defmethod read-object progn ((,objvar ,name) ,streamvar)
                                             (declare (ignorable ,streamvar))
                                             (with-slots ,(new-class-all-slots slots superclasses) ,objvar
                                               ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))))))

(defmacro define-tagged-binary-class (name (&rest superclasses) slots &rest options)
  (with-gensyms (typevar objvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
                                  (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
                                    (let* ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
                                      (let ((,objvar (make-instance
                                                      ,@(or (cdr (assoc :dispatch options))
                                                            (error "Must supply :dispatch form."))
                                                      ,@(mapcan #'slot->keyword-arg slots))))
                                        (read-object ,objvar ,streamvar)
                                        ,objvar))))))

(define-binary-type unsigned-integer (bytes bits-per-byte)
  (:reader (in)
           (loop with value = 0
;;                for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
                for low-bit from 0 upto (* bits-per-byte (1- bytes)) by bits-per-byte
                do (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
                finally (return value)))
  (:writer (out value)
;;           (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
           (loop for low-bit from 0 upto (* bits-per-byte (1- bytes)) by bits-per-byte
                do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))

(define-binary-type u1 () (unsigned-integer :bytes 1 :bits-per-byte 8))
(define-binary-type u2 () (unsigned-integer :bytes 2 :bits-per-byte 8))
(define-binary-type u3 () (unsigned-integer :bytes 3 :bits-per-byte 8))
(define-binary-type u4 () (unsigned-integer :bytes 4 :bits-per-byte 8))

(define-binary-class bmp-file-header ()
  ((bf-type u2)
   (bf-size u4)
   (bf-reserved-1 u2)
   (bf-reserved-2 u2)
   (bf-offbits u4)))

(define-binary-class bmp-info-header ()
  ((info-size u4)
   (info-width u4)
   (info-height u4)
   (info-planes u2)
   (info-bits u2)
   (info-compression u4)
   (info-image-size u4)
   (info-x-resolution u4)
   (info-y-resolution u4)
   (info-n-colors u4)
   (info-important-colors u4)))

(defun init-bmp-file-header (bf-obj data-size)
  (with-slots (bf-type bf-size bf-reserved-1 bf-reserved-2 bf-offbits) bf-obj
    (setf bf-type #x4d42
          bf-size (+ 54 data-size)
          bf-reserved-1 0
          bf-reserved-2 0
          bf-offbits 54))
  bf-obj)

(defun init-bmp-info-header-32 (info width height data-size)
  (with-slots (info-size info-width info-height info-planes info-bits
                         info-compression info-image-size
                         info-x-resolution info-y-resolution
                         info-n-colors info-important-colors) info
    (setf info-size 40
          info-width width
          info-height height
          info-planes 1
          info-bits 32
          info-compression 0
          info-image-size data-size
          info-x-resolution 11811
          info-y-resolution 11811
          info-n-colors 0
          info-important-colors 0))
  info)
    
(defun test-fill-bf-header (bf)
  (init-bmp-file-header bf 1024))

(defun test-fill-info-header (info)
  (init-bmp-info-header-32 info 16 16 1024))

(defun test ()
  (let ((bf (make-instance 'bmp-file-header))
        (info (make-instance 'bmp-info-header)))
    (with-open-file (out 
                     #P"/home/weida/quicklisp/local-projects/test.bmp"
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (write-object (test-fill-bf-header bf) out)
    (write-object (test-fill-info-header info) out)
    (dotimes (i (* 16 16))
      (write-byte 255 out) ;Blue
      (write-byte 0 out) ;Green
      (write-byte 0 out) ;Red
      (write-byte #xff out)))))
           
