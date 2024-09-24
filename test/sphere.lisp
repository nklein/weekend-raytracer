;;;; test/sphere.lisp

(in-package #:weekend-raytracer/test)

(nst:def-fixtures materials ()
  (lambertian (lambertian (color 1 1 1/2))))

(nst:def-test-group sphere-construction-tests (materials)
  (nst:def-test can-construct-a-sphere (:true)
    (sphere (vec 1 2 3) 4 blue-diffuse))

  (nst:def-test can-load-a-sphere (:true)
    #.(sphere (vec 1 2 3) 4 (lambertian (color 1 1 1/2)))))

(nst:def-test-group sphere-intersection-tests (materials)
  (nst:def-test can-intersect-sphere-from-outside (:true)
    (hit (sphere (vec 4 5) 3 blue-diffuse)
         (ray (vec 4 -1)
              (vec 0  1))
         (interval 0
                   10)))

  (nst:def-test can-intersect-sphere-from-inside (:true)
    (hit (sphere (vec 4 5) 3 blue-diffuse)
         (ray (vec 4  6)
              (vec 0  1))
         (interval 0
                   10)))

  (nst:def-test can-miss-sphere-from-outside (:equal nil)
    (hit (sphere (vec 4 5) 3 blue-diffuse)
         (ray (vec 4 -1)
              (vec 1  0))
         (interval 0
                   10)))

  (nst:def-test can-miss-sphere-because-of-min-max (:equal nil)
    (hit (sphere (vec 4 5) 3 blue-diffuse)
         (ray (vec 4 -1)
              (vec 0  1))
         (interval 0
                   1)))

  (nst:def-test normal-is-right-from-outside (:values :true (vec= #.(vec -1)))
    (let ((hit (to-full-hit (hit (sphere (vec 4) 3 blue-diffuse)
                                 (ray (vec 0)
                                      (vec 1))
                                 (interval 0
                                           10)))))
      (values (front-face-p hit)
              (normal hit))))

  (nst:def-test normal-is-right-from-inside (:values (:not :true) (vec= #.(vec -1)))
    (let ((hit (to-full-hit (hit (sphere (vec 4) 3 blue-diffuse)
                                 (ray (vec 4)
                                      (vec 1))
                                 (interval 0
                                           10)))))
      (values (front-face-p hit)
              (normal hit)))))
