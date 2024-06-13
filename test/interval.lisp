;;;; test/interval.lisp

(in-package #:weekend-raytracer/test)

(nst:def-test-group interval-construction-tests ()
  (nst:def-test can-construct-an-interval (:true)
    (interval 0 1)))

(nst:def-test-group interval-accessor-tests ()
  (nst:def-test can-destructure-an-interval (:values (:equalp 2) (:equalp 3))
    (with-interval (min max) (interval 2 3)
      (values min max))))
