;;; File: "arithmetic-dcls"
;;; Contents: Macros and type definitions for type-specific arithmetic
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Updated: Tue Jul  9 00:05:34 1996 by Jeff Dalton

(in-package :oplan-util)

;; Fixnum arithmetic
(export '(fix1+ fix1-))
(export '(fix+ fix- fix* fix-floor))
(export '(fix= fix> fix< fix>= fix<=))
(export '(fix-min fix-max fix-min2 fix-max2))

;; Double-float arithmetic
(export '(df+ df- df* df/))
(export '(df= df> df< df>= df<=))
(export '(number->double float->double))

;; Fixnum iterations
(export '(fix-dotimes do-vector-indices do-vector-elements))

;; 2-D arrays
(export '(numrows numcols))

;; 2-D arrays of double-floats
(export '(df-matrix))

;; Quantization
(export '(quantize-up quantize-down))


;;; Fixnum arithmetic

;;; N.B. If addition, multiplication, etc of more than 2 arguments is
;;; ever added, be sure to declare the type of intermediate results.

(defmacro fix1+ (i)
  `(the fixnum (1+ (the fixnum ,i))))

(defmacro fix1- (i)
  `(the fixnum (1- (the fixnum ,i))))

(defmacro fix+ (i j)
  `(the fixnum (+ (the fixnum ,i) (the fixnum ,j))))

(defmacro fix- (i j)
  `(the fixnum (- (the fixnum ,i) (the fixnum ,j))))

(defmacro fix* (i j)
  `(the fixnum (* (the fixnum ,i) (the fixnum ,j))))

(defmacro fix-floor (i j)
  `(the fixnum (floor (the fixnum ,i) (the fixnum ,j))))

(defmacro fix= (i j)
  `(= (the fixnum ,i) (the fixnum ,j)))

(defmacro fix> (i j)
  `(> (the fixnum ,i) (the fixnum ,j)))

(defmacro fix< (i j)
  `(< (the fixnum ,i) (the fixnum ,j)))

(defmacro fix>= (i j)
  `(>= (the fixnum ,i) (the fixnum ,j)))

(defmacro fix<= (i j)
  `(<= (the fixnum ,i) (the fixnum ,j)))

;;; Only 2-arg MIN and MAX are optimized by the AKCL compiler, even
;;; though there's no intermediate-result problem.  Hence the expansion
;;; via reduce below.  The same is true of the comparison functions.
;;; I don't know about other Lisps.

(defmacro fix-min (i j &rest more)
  (reduce #'(lambda (l r) `(fix-min2 ,l ,r))
	  (list* i j more)
	  :from-end t))

(defmacro fix-max (i j &rest more)
  (reduce #'(lambda (l r) `(fix-max2 ,l ,r))
	  (list* i j more)
	  :from-end t))

(defmacro fix-min2 (i j)
  `(the fixnum (min (the fixnum ,i) (the fixnum ,j))))

(defmacro fix-max2 (i j)
  `(the fixnum (max (the fixnum ,i) (the fixnum ,j))))


;;; Double-float arithmetic

(defmacro df+ (x y)
  `(the double-float (+ (the double-float ,x) (the double-float ,y))))

(defmacro df- (i j)
  `(the double-float (- (the double-float ,i) (the double-float ,j))))

(defmacro df* (x y)
  `(the double-float (* (the double-float ,x) (the double-float ,y))))

(defmacro df/ (x y)
  `(the double-float (/ (the double-float ,x) (the double-float ,y))))

(defmacro df= (i j)
  `(= (the double-float ,i) (the double-float ,j)))

(defmacro df> (i j)
  `(> (the double-float ,i) (the double-float ,j)))

(defmacro df< (i j)
  `(< (the double-float ,i) (the double-float ,j)))

(defmacro df>= (i j)
  `(>= (the double-float ,i) (the double-float ,j)))

(defmacro df<= (i j)
  `(<= (the double-float ,i) (the double-float ,j)))


(defmacro number->double (x)
  `(the double-float (float (the number ,x) 0d0)))

(defmacro float->double (x)
  `(the double-float (float (the float ,x) 0d0)))


;;; Fixnum iterations

(defmacro fix-dotimes ((var count &optional (result nil))
		       &body body)
  (let ((count-var (gensym)))
    `(let ((,count-var ,count))
       (declare (fixnum ,count-var))
       (do ((,var 0 (fix+ ,var 1)))
	   ((fix>= ,var ,count-var)
	    ,result)
	 (declare (fixnum ,var))
	 ,@body))))

(defmacro do-vector-indices ((var vec &optional (result nil))
			     &body body)
  `(fix-dotimes (,var (length (the vector ,vec))
		      ,@(if result (list result)))
     ,@body))

;;; Do-vector-elements exists in part because LOOP implementatins
;;; differ on what syntax they accept.  In particular, Lucid's LOOP
;;; doesn't accept ACROSS.

(defmacro do-vector-elements ((elt-var vec-form) &body body)
  (let ((v (gensym "V"))
	(i (gensym "I")))
    `(let ((,v ,vec-form))
       (fix-dotimes (,i (length (the simple-vector ,v)))
	 (let ((,elt-var (svref ,v ,i)))
	   ,@body)))))
  

;;; 2-D arrays

(defmacro numrows (mat)
  `(array-dimension ,mat 0))

(defmacro numcols (mat)
  `(array-dimension ,mat 1))


;;; 2-D arrays of double-floats

(deftype df-matrix (&optional rows cols)
  `(array double-float (,rows ,cols)))


;;; Macros for rounding to multiples of a granularity.

(defmacro quantize-up (n granularity)
  (let ((g (gensym)))
    `(let ((,g ,granularity))
       (* ,g (ceiling ,n ,g)))))

(defmacro quantize-down (n granularity)
  (let ((g (gensym)))
    `(let ((,g ,granularity))
       (* ,g (floor ,n ,g)))))

;;; End

