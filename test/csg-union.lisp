;;;; test/csg-union.lisp

(in-package #:weekend-raytracer/test)

(nst:def-test-group csg-union-construction-tests (materials-fixture)
  (nst:def-test can-construct-an-empty-csg-union (:true)
    (csg-union))

  (nst:def-test can-construct-length-one-csg-union (:true)
    (csg-union
     (sphere (vec 1 2 3) 4 blue-diffuse)))

  (nst:def-test can-construct-length-two-csg-union (:true)
    (csg-union
     (sphere (vec 1 2 3) 4 blue-diffuse)
     (sphere (vec 1 2 3) 2 blue-diffuse)))

  (nst:def-test can-load-a-csg-union (:true)
    #.(csg-union
       (sphere (vec 1 2 3) 4 (lambertian (color 1 1 1/2)))
       (sphere (vec 1 2 3) 2 (lambertian (color 1 1 1/2))))))

(nst:def-test-group csg-unions-tests (materials-fixture)
  (nst:def-test union-of-spread-out-spheres-is-hittable (real= 3)
    (tt (hit (csg-union
              (sphere (vec 4 5) 3 blue-diffuse)
              (sphere (vec 4 15) 2 blue-shiny))
             (ray (vec 4 -1)
                  (vec 0  1))
             (interval 0 100))))

  (nst:def-test union-of-spread-out-spheres-is-hittable-when-second-sphere-is-not-hittable (real= 3)
    (tt (hit (csg-union
              (sphere (vec 4 5) 3 blue-diffuse)
              (sphere (vec 15 4) 2 blue-shiny))
             (ray (vec 4 -1)
                  (vec 0  1))
             (interval 0 100))))

  (nst:def-test union-of-spread-out-spheres-is-hittable-when-first-sphere-is-not-hittable (real= 3)
    (tt (hit (csg-union
              (sphere (vec 15 4) 2 blue-shiny)
              (sphere (vec 4 5) 3 blue-diffuse))
             (ray (vec 4 -1)
                  (vec 0  1))
             (interval 0 100))))

  (nst:def-test union-of-nested-spheres-is-outer-when-outer-first (:values (real= 2) (real= 10))
    (destructuring-bind ((in . out)) (hit* (csg-union
                                            (sphere (vec 4 5) 4 blue-shiny)
                                            (sphere (vec 4 5) 3 blue-diffuse))
                                           (ray (vec 4 -1)
                                                (vec 0  1)))
      (values (tt in)
              (tt out))))

  (nst:def-test union-of-nested-spheres-is-outer-when-outer-second (:values (real= 2) (real= 10))
    (destructuring-bind ((in . out)) (hit* (csg-union
                                            (sphere (vec 4 5) 3 blue-diffuse)
                                            (sphere (vec 4 5) 4 blue-shiny))
                                           (ray (vec 4 -1)
                                                (vec 0  1)))
      (values (tt in)
              (tt out))))

  (nst:def-test union-of-overlapped-spheres-when-closer-first (:values (real= 3) (real= 12))
    (destructuring-bind ((in . out)) (hit* (csg-union
                                            (sphere (vec 4 5) 3 blue-diffuse)
                                            (sphere (vec 4 8) 3 blue-shiny))
                                           (ray (vec 4 -1)
                                                (vec 0  1)))
      (values (tt in)
              (tt out))))

  (nst:def-test union-of-overlapped-spheres-when-closer-second (:values (real= 3) (real= 12))
    (destructuring-bind ((in . out)) (hit* (csg-union
                                            (sphere (vec 4 8) 3 blue-shiny)
                                            (sphere (vec 4 5) 3 blue-diffuse))
                                           (ray (vec 4 -1)
                                                (vec 0  1)))
      (values (tt in)
              (tt out))))

  (nst:def-test union-of-three (:values (real= 3) (real= 15))
    (destructuring-bind ((in . out)) (hit* (csg-union
                                            (sphere (vec 4 5) 3 blue-diffuse)
                                            (sphere (vec 4 8) 3 blue-shiny)
                                            (sphere (vec 4 11) 3 blue-diffuse))
                                           (ray (vec 4 -1)
                                                (vec 0  1)))
      (values (tt in)
              (tt out))))


  (nst:def-test can-get-first-hit-from-an-union (real= 3)
    (tt (hit (csg-union
              (sphere (vec 4 5) 3 blue-diffuse)
              (sphere (vec 4 8) 3 blue-shiny))
             (ray (vec 4 -1)
                  (vec 0  1))
             (interval 0 10))))

  (nst:def-test can-get-second-hit-from-an-union (real= 12)
    (tt (hit (csg-union
              (sphere (vec 4 5) 3 blue-diffuse)
              (sphere (vec 4 8) 3 blue-shiny))
             (ray (vec 4 -1)
                  (vec 0  1))
             (interval 8 15))))

  )
