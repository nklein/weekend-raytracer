;;;; box-muller.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(defvar *box-muller-state* nil)

(declaim (type (function () vector-component-type) box-muller))
(defun box-muller (&optional u1 u2)
  (with-policy-expectations
      ((type (or null vector-component-type) u1 u2)
       (returns vector-component-type))
    (cond
      ((null *box-muller-state*)
       (let ((u1 (or u1 (random #.(vector-component 1))))
             (u2 (or u2 (random #.(vector-component 1)))))
         (declare (type vector-component-type u1 u2))
         (let ((rr (sqrt (* #.(vector-component -2) (log u1))))
               (aa (* u2 #.(vector-component (* 2 pi)))))
           (prog1
               (vector-component (* rr (cos aa)))
             (setf *box-muller-state* (vector-component (* rr (sin aa))))))))
      (t
       (prog1
           (the vector-component-type *box-muller-state*)
         (setf *box-muller-state* nil))))))

(defun reset-box-muller ()
  (or #+bordeaux-threads
      (bt:with-lock-held (*box-muller-lock*)
        (setf *box-muller-state* nil)
        t)
      (setf *box-muller-state* nil)))
