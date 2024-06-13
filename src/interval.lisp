;;;; interval.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(defstruct (interval (:conc-name %interval-)
                     (:constructor %make-interval (min max)))
  (min (error "Must provide MIN") :type vector-component-type :read-only t)
  (max (error "Must provide MAX") :type vector-component-type :read-only t))

(declaim (inline interval)
         (type (function (real real) interval) interval))
(defun interval (min max)
  (with-policy-expectations
      ((type real min max)
       (assertion (<= min max))
       (returns interval))
    (%make-interval (vector-component min) (vector-component max))))

(declaim (inline imin)
         (type (function (interval) vector-component-type) imin))
(defun imin (ii)
  (with-policy-expectations
      ((type interval ii)
       (returns vector-component-type))
    (%interval-min ii)))

(declaim (inline imax)
         (type (function (interval) vector-component-type) imax))
(defun imax (ii)
  (with-policy-expectations
      ((type interval ii)
       (returns vector-component-type))
    (%interval-max ii)))

(defmacro with-interval ((min max) interval &body body)
  (let ((ii (gensym "INTERVAL-")))
    `(let ((,ii ,interval))
       (let ((,min (imin ,ii))
             (,max (imax ,ii)))
         ,@body))))


(declaim (inline surroundsp)
         (type (function (interval real) boolean) surroundsp))
(defun surroundsp (tinterval tt)
  (with-policy-expectations
      ((type interval tinterval)
       (type real tt))
    (with-interval (tmin tmax) tinterval
      (<= tmin (vector-component tt) tmax))))
