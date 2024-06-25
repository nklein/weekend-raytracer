;;;; compile.lisp

(in-package #:weekend-raytracer)

(defmacro set-optimization-level ()
  '(declaim (optimize (speed 3) (safety 0))))

(set-optimization-level)

(defmacro with-policy-expectations (expectations &body body)
  `(policy-cond:with-expectations (> speed safety)
       ,expectations
     ,@body))
