;;;; test/halfspace.lisp

(in-package #:weekend-raytracer/test)

(nst:def-test-group halfspace-construction-tests (materials-fixture)
  (nst:def-test can-construct-a-halfspace (:values (vec= #.(unit-vector (vec 3 0 -4)))
                                                   (real= 1)
                                                   (:true))
    (with-halfspace (nn oo mm) (halfspace (vec 3 0 -4) 5 blue-diffuse)
      (values nn oo mm)))

  (nst:def-test can-construct-a-halfspace-with-a-point (:values (vec= #.(vec 1 0))
                                                                (real= 5)
                                                                (:true))
    (with-halfspace (nn oo mm) (halfspace (vec 1 0) (vec 5 9) blue-diffuse)
      (values nn oo mm)))

  (nst:def-test can-load-a-halfspace (:values (vec= #. (unit-vector (vec 0 -1 0)))
                                              (real= 4)
                                              (:true))
    (with-halfspace (nn oo mm) #.(halfspace (vec 0 -1 0) 4 (lambertian (color 1 1 1/2)))
      (values nn oo mm))))

(nst:def-test-group halfspace-intersection-tests (materials-fixture)
  (nst:def-test can-intersect-halfspace-from-outside (:true)
    (hit (halfspace (vec 0 -1) -3 blue-diffuse)
         (ray (vec 4 1)
              (vec 0 1))
         (interval 0
                   10)))

  (nst:def-test can-intersect-halfspace-from-inside (:true)
    (hit (halfspace (vec 0 1) 3 blue-diffuse)
         (ray (vec 4 1)
              (vec 0 1))
         (interval 0
                   10)))

  (nst:def-test can-miss-halfspace-from-outside (:equal nil)
    (hit (halfspace (vec 0 -1) -3 blue-diffuse)
         (ray (vec 4 -1)
              (vec 1 0))
         (interval 0
                   10)))

  (nst:def-test can-miss-halfspace-from-inside (:equal nil)
    (hit (halfspace (vec 0 1) 3 blue-diffuse)
         (ray (vec 4 -1)
              (vec 1 0))
         (interval 0
                   10)))

  (nst:def-test can-miss-halfspace-because-of-min-max (:equal nil)
    (hit (halfspace (vec 0 -1) -3 blue-diffuse)
         (ray (vec 4 -1)
              (vec 0  1))
         (interval 0
                   1)))

  (nst:def-test normal-is-right-from-outside (:values :true (vec= #.(vec -1)))
    (let ((hit (to-full-hit (hit (halfspace (vec -1) -1 blue-diffuse)
                                 (ray (vec 0)
                                      (vec 1))
                                 (interval 0
                                           10)))))
      (values (front-face-p hit)
              (normal hit))))

  (nst:def-test normal-is-right-from-inside (:values (:not :true) (vec= #.(vec 1)))
    (let ((hit (to-full-hit (hit (halfspace (vec -1) -1 blue-diffuse)
                                 (ray (vec 4)
                                      (vec -1))
                                 (interval 0
                                           10)))))
      (values (front-face-p hit)
              (normal hit)))))

(nst:def-test-group halfspace-all-intersections-tests (materials-fixture)
  (nst:def-test can-hit-halfspace-from-outside (:values (real= 2)
                                                        (:predicate plusp))
    (destructuring-bind ((in . out)) (hit* (halfspace (vec 0 1) 3 blue-diffuse)
                                           (ray (vec 4 5)
                                                (vec 0 -1)))
      (values (tt in)
              (tt out))))

  (nst:def-test can-hit-halfspace-from-inside (:values (:predicate minusp)
                                                       (real= 4))
    (destructuring-bind ((in . out)) (hit* (halfspace (vec 0 1) 3 blue-diffuse)
                                           (ray (vec 4 -1)
                                                (vec 0 1)))
      (values (tt in)
              (tt out))))

  (nst:def-test can-miss-halfspace-from-outside (:not :true)
    (hit* (halfspace (vec 0 1) 3 blue-diffuse)
          (ray (vec 4 5)
               (vec 1 0))))

  (nst:def-test can-miss-halfspace-surface-from-inside (:values (:predicate minusp)
                                                        (:predicate plusp))
    (destructuring-bind ((in . out)) (hit* (halfspace (vec 0 1) 3 blue-diffuse)
                                           (ray (vec 4 -1)
                                                (vec 1  0)))
      (values (tt in)
              (tt out)))))
