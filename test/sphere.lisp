;;;; test/sphere.lisp

(in-package #:weekend-raytracer/test)

(nst:def-test-group sphere-construction-tests ()
  (nst:def-test can-construct-a-unit-sphere (:true)
    (sphere (vec 1 2 3) 4))

  (nst:def-test can-load-a-unit-sphere (:true)
    #.(sphere (vec 1 2 3) 4)))

#+(or)
(nst:def-test-group sphere-intersection-tests ()
  (nst:def-test can-intersect-sphere (:true)
    (hit-sphere (sphere (vec 4 5) 3)
                (ray (vec 4 -1)
                     (vec 0  1))))

  (nst:def-test can-miss-sphere (:equal nil)
    (hit-sphere (sphere (vec 4 5) 3)
                (ray (vec 4 -1)
                     (vec 1  0)))))
