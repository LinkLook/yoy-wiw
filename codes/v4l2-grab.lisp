
(defvar *capture-device* "/dev/video0")
(defparameter *capture-fd* nil)
;;(defparameter *render-thread-stop* (bt:make-condition-variable))
(defparameter *cap-thread-stop* NIL)
(defparameter *camera-data* nil)

;; what we want from camera
(defvar *want-width* 600)
(defvar *want-height* 400)

;; what we really get
(defparameter *got-width* nil)
(defparameter *got-height* nil)

(defmacro without-errors (&body body)
  `(handler-case (progn ,@body)
     (error (c) (format t "suppressed error: ~A~%" c) nil)))

(defun char-at (pos data)
  (code-char (ldb (byte 8 (* 8 pos)) data)))

(defun format-string (pixfmt)
  (format nil "~C~C~C~C"
	  (char-at 0 pixfmt)
	  (char-at 1 pixfmt)
	  (char-at 2 pixfmt)
	  (char-at 3 pixfmt)))

(defun diagnose (fd)
  (let ((caps (v4l2:query-capabilities fd)))
    (format t (v4l2:%device-info caps))
    (unless (v4l2:capable caps v4l2:cap-video-capture)
      (error "not a capture device"))
    (unless (v4l2:capable caps v4l2:cap-streaming)
      (error "not a streaming device"))
    (when (v4l2:capable caps v4l2:cap-tuner)
      (without-errors
	  (loop for idx from 0 do
	       (progn
		 (v4l2:get-tuner-params fd idx)
		 ;; show tuner params
		 ))))

    (without-errors
	(loop for idx from 0 do
	     (with-slots (v4l2:index v4l2:name v4l2:type v4l2:tuner)
		 (v4l2:get-input-params fd idx)
	       (format t "input [~D] name: ~A, type ~A~%"
		       v4l2:index
		       v4l2:name
		       (if (= v4l2:type v4l2:input-type-tuner) "tuner" "camera"))
	       (when (= v4l2:type v4l2:input-type-tuner)
		 (format t "input [~D] connected to tuner ~D~%" v4l2:index v4l2:tuner))

	       (without-errors
		   (loop for idx1 from 0 do
			(with-slots (v4l2:index v4l2:name)
			    (v4l2:get-input-standard fd idx1)
			  (format t "input [~D] std [~D] name: ~A~%"
				  idx v4l2:index v4l2:name)))))))

    (v4l2:set-input fd 0)		; some cameras don't set input by default

    (without-errors
	(loop for idx from 0 do
	     (with-slots (v4l2:index v4l2:pixelformat) (v4l2:get-format fd idx)
	       (format t "format [~D] ~S~%" v4l2:index
		       (format-string v4l2:pixelformat)))))))

(defun video-uninit (fd buffers)
  (v4l2:stream-off fd)			; stop capturing
  (v4l2:unmap-buffers buffers)		; throw away buffers from memory
  (isys:close fd)				; close device
  (format t "that's all!~%"))

(defun video-init (device)
  (let ((fd (isys:open device isys:o-rdwr)))
    (setq *capture-fd* fd)
    (diagnose fd)					; info about device
    (device-init fd)					; setup
    (let ((buffers (v4l2:map-buffers fd 4)))		; map 4 buffers into memory
      
      (dolist (buf buffers)
        (dolist (e buf)
          (format t "~A|" e))
        (format t "~%"))
          
      (v4l2:stream-on fd buffers)			; start capturing
      (values fd buffers))))

(defun write-camera-data-to-bmp-file (file-name)
  (with-open-file (out file-name
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (let ((bf (init-bmp-file-header (make-instance 'bmp-file-header) (length *camera-data*)))
          (info (init-bmp-info-header-32 
                 (make-instance 'bmp-info-header) 
                 *got-width* *got-height* (length *camera-data*))))
      (write-object bf out)
      (write-object info out)
      (write-sequence *camera-data* out))))
           

(defun capture-thread ()
  (format t "cap thread start~%")
  (multiple-value-bind (fd buffers)
      (video-init *capture-device*)
    (loop thereis *cap-thread-stop* do
	 (let ((frame (without-errors (v4l2:get-frame fd))))		; get one frame from driver
	   (when frame			; errors from v4l2convert.so are highly possible
         (time (fix-camera-data frame buffers))
;;	     (when *camera-widget*
;;	       (with-main-loop
;;               (widget-queue-draw *camera-widget*)))
         (write-camera-data-to-bmp-file  #P"/tmp/camera0.bmp")

         (setf *cap-thread-stop* T)

	     (v4l2:put-frame fd frame))))	; put frame back to driver
    (video-uninit fd buffers))
  (format t "cap thread exit~%"))

(defun fix-camera-data (frame buffers)
  (format t "frame=~D " frame)
  (multiple-value-bind (buffer address length)
      (values-list (nth frame buffers))
    ;;	       (declare (ignore buffer))
    (format t "~A|~A|~D~%" buffer address length)
    ;; Silly rgb24->rgb32 converter
    ;;	       (bt:with-lock-held (*camera-data-lock*)
    (declare (optimize (speed 3) (debug 0) (safety 0)))
    (let ((pixels (floor (/ length 4))))
      (loop for i fixnum from 0 below pixels do
           (let* ((y0 (cffi:mem-aref address :uchar (+ (* 4 i) 0)))
                  (u (cffi:mem-aref address :uchar (+ (* 4 i) 1)))
                  (y1 (cffi:mem-aref address :uchar (+ (* 4 i) 2)))
                  (v (cffi:mem-aref address :uchar (+ (* 4 i) 3)))
                  (pix-1 (yuv2rgb y0 u v))
                  (pix-2 (yuv2rgb y1 u v)))

             (setf (aref *camera-data* (+ (* 8 (- pixels i 1)) 0)) (third pix-2)
                   (aref *camera-data* (+ (* 8 (- pixels i 1)) 1)) (second pix-2)
                   (aref *camera-data* (+ (* 8 (- pixels i 1)) 2)) (first pix-2)
                   (aref *camera-data* (+ (* 8 (- pixels i 1)) 3)) #xff
                   (aref *camera-data* (+ (* 8 (- pixels i 1)) 4)) (third pix-1)
                   (aref *camera-data* (+ (* 8 (- pixels i 1)) 5)) (second pix-1)
                   (aref *camera-data* (+ (* 8 (- pixels i 1)) 7)) #xff
                   (aref *camera-data* (+ (* 8 (- pixels i 1)) 6)) (first pix-1)))))))

(defun yuv2rgb (y u v)
  (mapcar #'(lambda (x) (ldb (byte 8 0) (round x))) 
          (list (+ y (* 1.13983 (- v 128)))
                (- y (* 0.39465 (- u 128)) (* 0.58060 (- v 128)))
                (+ y (* 2.03211 (- u 128))))))

(defun device-init (fd)
  (v4l2:set-input fd 0)
  (without-errors
      (v4l2:set-control fd v4l2:cid-exposure 0.05))
  (format t "set ~Dx~D, format ~S~%" *want-width* *want-height*
	  (format-string v4l2:pix-fmt-rgb24))
  (v4l2:set-image-format fd *want-width* *want-height* v4l2:pix-fmt-rgb24)
  (with-slots (v4l2:width v4l2:height v4l2:sizeimage v4l2:pixelformat)
      (v4l2:format-pix (v4l2:get-image-format fd))
    (setf *got-width* v4l2:width)
    (setf *got-height* v4l2:height)
    (format t "got ~Dx~D size ~D, format ~S~%" *got-width* *got-height*
            v4l2:sizeimage (format-string v4l2:pixelformat))
    (setq *camera-data* (make-array (* 4 *got-height* *got-width*)
				    :element-type '(unsigned-byte 8)
				    :initial-element #xff))))


(capture-thread)
