;;;; ray.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(deftype ray () '(cons vec vec))

(declaim (inline ray)
         (type (function (vec vec) ray) ray))
(defun ray (origin direction)
  (with-policy-expectations
      ((type vec origin direction)
       (assertion (= (length origin) (length direction)))
       (returns ray))
    (cons origin direction)))

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
    (car ray)))

(declaim (inline direction)
         (type (function (ray) vec) direction))
(defun direction (ray)
  (with-policy-expectations
      ((type ray ray)
       (returns vec))
    (cdr ray)))

(declaim (inline at)
         (type (function (ray real) vec)))
(defun at (ray tt)
  (with-policy-expectations
      ((type ray ray)
       (type real tt)
       (returns vec))
    (let ((origin (origin ray))
          (direction (direction ray)))
      (with-policy-expectations
          ((type vec origin direction))
        (v+ origin
            (v* direction
                tt))))))
