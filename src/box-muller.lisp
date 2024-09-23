;;;; box-muller.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)


(declaim (type (function () vector-component-type) box-muller))
(let ((z1 nil))
  (declare (type (or null vector-component-type) z1))
  (defun box-muller (&optional u1 u2)
    (with-policy-expectations
        ((type (or null vector-component-type) u1 u2)
         (returns vector-component-type))
      (cond
        ((null z1)
         (let ((u1 (or u1 (random #.(vector-component 1))))
               (u2 (or u2 (random #.(vector-component 1)))))
           (declare (type vector-component-type u1 u2))
           (let ((rr (sqrt (* #.(vector-component -2) (log u1))))
                 (aa (* u2 #.(vector-component (* 2 pi)))))
             (prog1
                 (vector-component (* rr (cos aa)))
               (setf z1 (vector-component (* rr (sin aa))))))))
        (t
         (prog1
             z1
           (setf z1 nil))))))

  (defun reset-box-muller ()
    (setf z1 nil)))
