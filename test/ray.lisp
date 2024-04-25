;;;; test/ray.lisp

(in-package #:weekend-raytracer/test)

(nst:def-test-group ray-construction-tests ()
  (nst:def-test can-construct-a-ray (:true)
    (ray (vec 1 2 3) (vec 1 0 0)))

  (nst:def-test constructor-returns-a-ray (:true)
    (typep (ray (vec 1 2 3) (vec 1 0 0)) 'ray))

  (nst:def-test constructor-satisfies-rayp (:true)
    (rayp (ray (vec 1 2 3) (vec 1 0 0)))))

(nst:def-test-group ray-accessor-tests ()
  (nst:def-test origin (:equalp #.(vec 1 2 3))
    (origin (ray (vec 1 2 3) (vec 1 0 0))))

  (nst:def-test direction (:equalp #.(vec 1 0 0))
    (direction (ray (vec 1 2 3) (vec 1 0 0))))

  (nst:def-test at (:equalp #.(vec 3 5))
    (at (ray (vec 1 9) (vec 1 -2)) 2)))
