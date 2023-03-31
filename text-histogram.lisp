(in-package :text-histogram)

;; This Common Lisp implementation was directly based on the Python
;; implementation accessible at
;; <https://github.com/basnijholt/text_histogram3>. That code, in turn,
;; is based on Bit.ly's data_hacks
;; <https://github.com/bitly/data_hacks> histogram.py code.

;; The original bit.ly code is Copyright 2010 Bitly

;;;
;;; Generate a text format histogram.
;;;

;; bar character to be used for histogram bars
(defparameter bar-char
  #\|
  ;; unicode characters:
  ;;   #\END_OF_PROOF	;0x220E ; character: ∎
  ;;   #\BLACK_SQUARE             ; character: ■
  ;;   #\SQUARE_WITH_DIAGONAL_CROSSHATCH_FILL ; character: ▩
  )

;; A class that facilitates calculating a running mean, variance, and
;; standard deviation
(defclass mvsd ()
  ((is_started :accessor mvsd-is_started
       :initarg :is_started
       :initform nil
       :type boolean
       :documentation "is_started")
   (ss :accessor mvsd-ss
       :initarg :ss
       :initform 0.0
       :type float
       :documentation "(running) sum of square deviations from mean")
   (m :accessor mvsd-m
       :initarg :m
       :initform 0.0
       :type float
       :documentation "(running) mean")
   (total_w :accessor mvsd-total_w
       :initarg :total_w
       :initform 0.0
       :type float
       :documentation "# weight of items seen")
   ))

;; add another datapoint to the mvsd object
(defmethod add (x (mvsd mvsd) w)
  (declare (number x))
  (let ((w (or w 1)))
    (cond ((not (mvsd-is_started mvsd))
	   (setf (mvsd-is_started mvsd) t)
	   (setf (mvsd-m mvsd) x)
	   (setf (mvsd-total_w mvsd) w)
	   (setf (mvsd-ss mvsd) 0.0))
	  (t
	   (let ((temp_w (+ (mvsd-total_w mvsd) w)))
	     (setf (mvsd-ss mvsd)
		   (+ (mvsd-ss mvsd)
		      (/ (* (mvsd-total_w mvsd)
			    w
			    (- x (mvsd-m mvsd))
			    (- x (mvsd-m mvsd)))
			 temp_w)))
	     (setf (mvsd-m mvsd)
		   (+ (mvsd-m mvsd)
		      (/ (- x (mvsd-m mvsd))
			 temp_w)))
	     (setf (mvsd-total_w mvsd)
		   temp_w))))))

(defmethod var ((mvsd mvsd))
  "Return NIL if total_w isn't > 0."
  (cond ((> (mvsd-total_w mvsd) 0)
	 (/ (mvsd-ss mvsd)
	    (mvsd-total_w mvsd)))
	(t nil)))

(defmethod sd ((mvsd mvsd))
  "Return NIL if total_w isn't > 0."
  (cond ((> (mvsd-total_w mvsd) 0)
	 (sqrt (var mvsd)))
	(t nil)))

(defmethod mean ((mvsd mvsd))
  (mvsd-m mvsd))

;; BUCKETS:     number of buckets to use for the histogram
;; CUSTBUCKETS: list of bucket edges (numbers) for the histogram
;; CALC_MSVD:   calculate and display mean, variance and SD.
;; DATA:        a set of numbers
;; MINIMUM:     NIL or minimum value for graph
;; MAXIMUM:     NIL or maximum value for graph
(defun histogram (data
		  &key minimum maximum buckets custbuckets
		    (calc_msvd-p t)
		    (stream *standard-output*))
  "Generate the text histogram."
  (declare (optimize (debug 3) (safety 3)))
  (let ((bucket-scale 1)
	(min_v (or minimum (apply 'min data)))
	(max_v (or maximum (apply 'max data))))
    (declare (integer bucket-scale))
    (assert (>= max_v min_v))
    (let ((diff (- max_v min_v))
	  (boundaries nil)
	  (bucket-counts nil))
      (declare (list bucket-counts))
      (cond (custbuckets
	     (let ((sorted-bounds
		     (sort custbuckets '<) ;ascending sort
		     ))
	       (if (< (car (last sorted-bounds)) max_v)
		   (setf (car (last sorted-bounds)) max_v))
	       (loop for x in sorted-bounds
		     do (cond ((and (>= x min_v) (<= x max_v))
			       (setf boundaries (append boundaries (list x))))
			      ((<= x max_v)
			       (setf boundaries (append boundaries
							(list max_v)))))
		     do (setf bucket-counts
			      (make-list (length boundaries)
					 :initial-element 0))
		     do (setf buckets (length boundaries)))))
	    (t
	     (setf buckets (or buckets 10))
	     (assert (> buckets 0))
	     (let ((step (/ diff buckets)))
	       (setf bucket-counts
		     (make-list buckets :initial-element 0))
	       (loop for x from 0 to (1- buckets)
		     do (setf boundaries
			      (append boundaries
				      (list (+ min_v
					       (* step
						  (+ x 1))))))))))
      (let ((skipped 0)
	    (samples 0)
	    (mvsd (make-instance 'mvsd))
	    (accepted-data nil))
	(declare (list accepted-data))
	(loop for value in data
	      do (incf samples)
	      if calc_msvd-p
		do (progn (add value mvsd nil) ; !!
			  (setf accepted-data
				(append accepted-data
					(list value))))
	      if (or (< value min_v) (> value max_v))
		do (incf skipped) ; go back to loop if we make it to here
	      else
		do (let ((bucket-position 0))
		     (loop for boundary in boundaries
			   if (<= value boundary)
			     do (progn (incf (elt bucket-counts bucket-position))
				       (return))
			   do (incf bucket-position))))
	;; auto-pick the hash scale
	(let ((bucket-counts-max (apply 'max bucket-counts)))
	  (if (> bucket-counts-max 75)
	      (setf bucket-scale
		    (floor (/ bucket-counts-max 75)))))
	(format stream
		"# NumSamples = ~S; Min = ~S; Max = ~S~%"
		samples min_v max_v)
	(when skipped
          (format stream "# ~A value~A outside of min/max~%"
		  (if (>= skipped 1) skipped "No")
		  (if (> skipped 1) "s" "")))
	(when calc_msvd-p
	  (format stream "# Mean = ~S; Variance = ~S; SD = ~S~%" ;Median ~S
		  (float (mean mvsd))
		  (var mvsd)
		  (sd mvsd)
		  ;(median accepted_data)
		  ))
	(format stream
		"# each ~A represents a count of ~S~%"
		bar-char
		bucket-scale)
	(let ((bucket-min min_v)	; ? - why set this if we set it below
	      (bucket-max max_v)
	      bucket-count)
	  (loop for bucket from 0 to (1- buckets)
		for star-count = 0
		do (setf bucket-max (elt boundaries bucket))
		do (setf bucket-count (elt bucket-counts bucket))
		if (> bucket-count 0)
		  do (setf star-count (/ bucket-count bucket-scale))
		do (format stream "~10,4F - ~10,4F [~6D]: ~A~%"
			   bucket-min bucket-max bucket-count
			   (make-string star-count
					:element-type 'character
					:initial-element bar-char))
		do (setf bucket-min bucket-max)
		))))))
