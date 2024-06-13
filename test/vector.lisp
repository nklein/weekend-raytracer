;;;; test/vector.lisp

(in-package #:weekend-raytracer/test)

(nst:def-test-group vector-construction-tests ()
  (nst:def-test construct-a-3d-vector (:true)
    (vec 1 2 3))

  (nst:def-test construct-a-4d-vector (:true)
    (vec 1 2 3 4))

  (nst:def-test constructor-returns-a-vec (:true)
    (typep (vec 1 2 3) 'vec))

  (nst:def-test constructor-satisfies-vecp (:true)
    (vecp (vec 1 2 3)))

  (nst:def-test constructor-upgrades-types (:true)
    (typep (vref (vec 1) 0) 'vector-component-type)))

(nst:def-test-group vector-math-tests ()
  (nst:def-test v+-test (:equalp '(2 5))
    (mapv #'round
          (v+ (vec 1 2)
              (vec 1 3))))

  (nst:def-test v--test (:equalp '(2 5))
    (mapv #'round
          (v- (vec 3 8)
              (vec 1 3))))

  (nst:def-test v*-test (:equalp '(2 4))
    (mapv #'round
          (v* (vec 1 2)
              2)))

  (nst:def-test v/-test (:equalp '(2 4))
    (mapv #'round
          (v/ (vec 6 12)
              3)))

  (nst:def-test vlen^2 (:equalp 25)
    (values (round (vlen^2 (vec 3 4)))))

  (nst:def-test vlen (:equalp 5)
    (values (round (vlen (vec 3 4)))))

  (nst:def-test v. (:equalp 10)
    (v. (vec 1 3) (vec -2 4))))
