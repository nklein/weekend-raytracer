;;;; vector.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(defstruct (vec (:conc-name %vec-)
                (:constructor %make-vec (vals)))
  (vals (error "Must specify VALS") :type (simple-array vector-component-type 1) :read-only t)
  (vlen^2 nil :type (or null vector-component-type))
  (vlen nil :type (or null vector-component-type)))

(defmethod make-load-form ((object vec) &optional environment)
  (declare (ignore environment))
  `(vec ,@(map 'list #'identity (%vec-vals object))))

(declaim (inline %vec)
         (type (function (list) vec) %vec))
(defun %vec (vals)
  (with-policy-expectations
      ((type list vals)
       (assertion (every #'realp vals))
       (returns vec))
    (%make-vec (make-array (list (length vals))
                           :element-type 'vector-component-type
                           :initial-contents (mapcar #'vector-component vals)))))

(declaim (inline vec)
         (type (function (&rest real) vec)))
(defun vec (&rest vals)
  (with-policy-expectations
      ((assertion (every #'realp vals))
       (returns vec))
    (%vec vals)))

(declaim (inline vecp)
         (type (function (t) boolean) vecp))
(defun vecp (x)
  (with-policy-expectations
      ((returns boolean))
    (typep x 'vec)))

(declaim (inline vref)
         (type (function (vec spatial-dimension-index-type) vector-component-type) vref))
(defun vref (vec index)
  (with-policy-expectations
      ((type vec vec)
       (type spatial-dimension-index-type index)
       (returns vector-component-type))
    (aref (%vec-vals vec) index)))

(declaim (inline vsize)
         (type (function (vec) spatial-dimensions-type) vsize))
(defun vsize (v)
  (with-policy-expectations
      ((type vec v)
       (returns spatial-dimensions-type))
    (array-dimension (%vec-vals v) 0)))

(defmacro element-wise2 (fn a b)
  (let ((ff (gensym "FF-"))
        (aa (gensym "AA-"))
        (bb (gensym "BB-"))
        (av (gensym "AV-"))
        (bv (gensym "BV-"))
        (cv (gensym "CV-"))
        (ii (gensym "I-")))
    `(let ((,ff ,fn)
           (,aa ,a)
           (,bb ,b))
       (with-policy-expectations
           ((type vec ,aa)
            (type vec ,bb)
            (returns vec))
         (let* ((,av (%vec-vals ,aa))
                (,bv (%vec-vals ,bb))
                (,cv (make-array (list (vsize ,aa))
                                 :element-type 'vector-component-type
                                 :initial-element (vector-component 0))))
           (loop :for ,ii :below (vsize ,aa)
                 :do (setf (aref ,cv ,ii)
                           (funcall ,ff (aref ,av ,ii) (aref ,bv ,ii))))
           (%make-vec ,cv))))))

(defmacro element-wise1 (fn a)
  (let ((ff (gensym "FF-"))
        (aa (gensym "AA-"))
        (av (gensym "AV-"))
        (cv (gensym "CV-"))
        (ii (gensym "I-")))
    `(let ((,ff ,fn)
           (,aa ,a))
       (with-policy-expectations
           ((type vec ,aa)
            (returns vec))
         (let* ((,av (%vec-vals ,aa))
                (,cv (make-array (list (vsize ,aa))
                                 :element-type 'vector-component-type
                                 :initial-element (vector-component 0))))
           (loop :for ,ii :below (vsize ,aa)
                 :do (setf (aref ,cv ,ii)
                           (funcall ,ff (aref ,av ,ii))))
           (%make-vec ,cv))))))

(defmacro element-wise (fn a &optional (b nil b-p))
  (if b-p
      `(element-wise2 ,fn ,a ,b)
      `(element-wise1 ,fn ,a)))

(declaim (inline v+)
         (type (function (vec vec) vec) v+))
(defun v+ (a b)
  (element-wise #'+ a b))

#+(or)
(let ((a (vec 1 2 3 4 5 6 7 8 9 10))
      (b (vec 1 2 3 4 5 6 7 8 9 10)))
  (time (loop :repeat 1000000
              :do (v+ a b))))

(declaim (inline v-)
         (type (function (vec vec) vec) v-))
(defun v- (a b)
  (element-wise #'- a b))

(declaim (inline v*v)
         (type (function (vec vec) vec) v*v))
(defun v*v (a b)
  (element-wise #'* a b))

(declaim (inline v*)
         (type (function (vec real) vec) v*))
(defun v* (vec scalar)
  (with-policy-expectations
      ((type real scalar))
    (let ((ss (vector-component scalar)))
      (element-wise
          (lambda (x)
            (declare (type vector-component-type x))
            (* x ss))
          vec))))

(declaim (inline v/)
         (type (function (vec real) vec) v/))
(defun v/ (vec scalar)
  (with-policy-expectations
      ((type real scalar)
       (assertion (not (zerop scalar))))
    (element-wise (lambda (x) (/ x scalar)) vec)))

(declaim (inline vlen^2)
         (type (function (vec) vector-component-type) vlen^2))
(defun vlen^2 (vec)
  (with-policy-expectations
      ((type vec vec)
       (returns vector-component-type))
    (or (%vec-vlen^2 vec)
        (setf (%vec-vlen^2 vec)
              (reduce #'+ (%vec-vals vec)
                      :key (lambda (x)
                             (declare (type vector-component-type x))
                             (* x x)))))))

(declaim (inline vlen)
         (type (function (vec) vector-component-type) vlen))
(defun vlen (vec)
  (with-policy-expectations
      ((type vec vec)
       (returns vector-component-type))
    (or (%vec-vlen vec)
        (setf (%vec-vlen vec)
              (the vector-component-type (sqrt (vlen^2 vec)))))))

(declaim (inline unit-vector)
         (type (function (vec) vec) unit-vector))
(defun unit-vector (vec)
  (with-policy-expectations
      ((type vec vec)
       (returns vec))
    (let ((ret (v/ vec (vlen vec))))
      (setf (%vec-vlen^2 ret) #.(vector-component 1)
            (%vec-vlen ret) #.(vector-component 1))
      ret)))

(declaim (inline random-unit-vector)
         (type (function (spatial-dimensions-type) vec)))
(defun random-unit-vector (spatial-dimensions)
  (with-policy-expectations
      ((type spatial-dimensions-type spatial-dimensions)
       (returns vec))
    (unit-vector (%vec (loop :repeat spatial-dimensions
                             :collecting (box-muller))))))

(declaim (inline random-unit-vector-on-hemisphere)
         (type (function (vec) vec)))
(defun random-unit-vector-on-hemisphere (normal)
  (with-policy-expectations
      ((type vec normal)
       (returns vec))
    (let ((spatial-dimensions (vsize normal)))
      (let ((vec (unit-vector (%vec (loop :repeat spatial-dimensions
                                          :collecting (box-muller))))))
        (if (plusp (v. vec normal))
            vec
            (v* vec #.(vector-component -1)))))))

(declaim (inline v.)
         (type (function (vec vec) vector-component-type) v.))
(defun v. (a b)
  (with-policy-expectations
      ((type vec a)
       (type vec b)
       (assertion (= (vsize a) (vsize b)))
       (returns vector-component-type))
    (let ((av (%vec-vals a))
          (bv (%vec-vals b)))
      (loop :for ii :below (vsize a)
            :summing (* (aref av ii) (aref bv ii)) :of-type vector-component-type))))

(defmacro mapv (fn &rest vs)
  (let ((func (gensym "FUNC-"))
        (vecs (gensym "VECS-")))
    `(let ((,func ,fn)
           (,vecs (list ,@vs)))
       (with-policy-expectations
           ((type function ,func)
            (assertion (every #'vecp ,vecs))))
       (apply #'map
              'list
              ,func
              (mapcar #'%vec-vals ,vecs)))))

(declaim (inline near-zero)
         (type (function (vec) boolean)))
(defun near-zero (vec)
  (with-policy-expectations
      ((type vec vec)
       (returns boolean))
    (loop :with epsilon := #.(vector-component 1/10000000)
          :with ret := t
          :while ret
          :for ii :below (vsize vec)
          :unless (< (- epsilon) (vref vec ii) epsilon)
            :do (setf ret nil)
          :finally (return ret))))
