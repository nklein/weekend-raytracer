;;;; test/criteria.lisp

(in-package #:weekend-raytracer/test)


(nst:def-criterion (vec= (given &optional (tolerance 1/100000)) (actual))
  (let ((mag^2 (vlen^2 (v- actual given))))
    (cond
      ((< mag^2 tolerance)
       (nst:make-success-report))
      (t
       (nst:make-failure-report :format "Actual vector ~A does not match given ~A"
                                :args (list actual given))))))
