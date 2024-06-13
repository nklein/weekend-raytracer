;;;; ray.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(defstruct (ray (:conc-name %ray-)
                (:constructor %make-ray (origin direction)))
  (origin (error "Must specify origin") :type vec :read-only t)
  (direction (error "Must specify direction") :type vec :read-only t))

(defmethod make-load-form ((object ray) &optional environment)
  `(ray ,(make-load-form (%ray-origin object) environment)
        ,(make-load-form (%ray-direction object) environment)))

(declaim (inline ray)
         (type (function (vec vec) ray) ray))
(defun ray (origin direction)
  (with-policy-expectations
      ((type vec origin direction)
       (assertion (= (vsize origin) (vsize direction)))
       (returns ray))
    (%make-ray origin direction)))

(declaim (inline rayp))
(defun rayp (x)
  (with-policy-expectations
      ((returns boolean))
    (typep x 'ray)))

(declaim (inline origin)
         (type (function (ray) vec) origin))
(defun origin (ray)
  (with-policy-expectations
      ((type ray ray)
       (returns vec))
    (%ray-origin ray)))

(declaim (inline direction)
         (type (function (ray) vec) direction))
(defun direction (ray)
  (with-policy-expectations
      ((type ray ray)
       (returns vec))
    (%ray-direction ray)))

(defmacro with-ray ((origin direction) ray &body body)
  (let ((rr (gensym "RR-")))
    `(let ((,rr ,ray))
       (let ((,origin (origin ,rr))
             (,direction (direction ,rr)))
         ,@body))))

(declaim (inline at)
         (type (function (ray real) vec)))
(defun at (ray tt)
  (with-policy-expectations
      ((type ray ray)
       (type real tt)
       (returns vec))
    (with-ray (origin direction) ray
      (with-policy-expectations
          ((type vec origin direction))
        (v+ origin
            (v* direction
                tt))))))
