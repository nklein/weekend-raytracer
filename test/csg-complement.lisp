;;;; test/csg-complement.lisp

(in-package #:weekend-raytracer/test)

(nst:def-test-group csg-complement-construction-tests (materials-fixture)
  (nst:def-test can-construct-csg-complement (:true)
    (csg-complement
     (sphere (vec 1 2 3) 4 blue-diffuse)))

  (nst:def-test can-load-a-csg-complement (:true)
    #.(csg-complement
       (sphere (vec 1 2 3) 2 (lambertian (color 1 1 1/2))))))

(nst:def-test-group csg-complement-tests (materials-fixture)
  (nst:def-test can-miss-sphere-entirely (:not :true)
    (hit (csg-complement
          (sphere (vec 4 5) 3 blue-shiny))
         (ray (vec 8 -1)
              (vec 0  1))
         (interval 0 1000)))

  (nst:def-test complement-of-sphere-from-outside (:values (real= 3)
                                                           (vec= #. (vec 0 -1)))
    (let ((hit (hit (csg-complement
                     (sphere (vec 4 5) 3 blue-shiny))
                    (ray (vec 4 -1)
                         (vec 0  1))
                    (interval 0 1000))))
      (values (tt hit)
              (normal (to-full-hit hit)))))

  (nst:def-test complement-of-sphere-from-inside (:values (real= 3)
                                                          (vec= #. (vec 0 -1)))
    (let ((hit (hit (csg-complement
                     (sphere (vec 4 5) 3 blue-shiny))
                    (ray (vec 4 -1)
                         (vec 0  1))
                    (interval 0 1000))))
      (values (tt hit)
              (normal (to-full-hit hit)))))

  (nst:def-test complement-of-hit*-of-sphere (:values (real= #.most-negative-vector-component-type)
                                                      (real= 3)
                                                      (real= 9)
                                                      (real= #.most-positive-vector-component-type))
    (let ((hits (hit* (csg-complement
                       (sphere (vec 4 5) 3 blue-shiny))
                      (ray (vec 4 -1)
                           (vec 0  1)))))
      (values (tt (car (first hits)))
              (tt (cdr (first hits)))
              (tt (car (second hits)))
              (tt (cdr (second hits)))))))
