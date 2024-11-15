;;;; test/csg-intersect.lisp

(in-package #:weekend-raytracer/test)

(nst:def-test-group csg-intersection-construction-tests (materials-fixture)
  (nst:def-test can-construct-an-empty-csg-intersection (:true)
    (csg-intersection))

  (nst:def-test can-construct-length-one-csg-intersection (:true)
    (csg-intersection
     (sphere (vec 1 2 3) 4 blue-diffuse)))

  (nst:def-test can-construct-length-two-csg-intersection (:true)
    (csg-intersection
     (sphere (vec 1 2 3) 4 blue-diffuse)
     (sphere (vec 1 2 3) 2 blue-diffuse)))

  (nst:def-test can-load-a-csg-intersection (:true)
    #.(csg-intersection
       (sphere (vec 1 2 3) 4 (lambertian (color 1 1 1/2)))
       (sphere (vec 1 2 3) 2 (lambertian (color 1 1 1/2))))))

(nst:def-test-group csg-intersections-tests (materials-fixture)
  (nst:def-test intersection-of-spread-out-spheres-is-nil (:not :true)
    (hit* (csg-intersection
           (sphere (vec 4 5) 3 blue-diffuse)
           (sphere (vec 4 15) 2 blue-shiny))
          (ray (vec 4 -1)
               (vec 0  1))))

  (nst:def-test intersection-of-nested-spheres-is-inner-when-inner-first (:values (real= 3) (real= 9))
    (destructuring-bind ((in . out)) (hit* (csg-intersection
                                            (sphere (vec 4 5) 3 blue-diffuse)
                                            (sphere (vec 4 5) 4 blue-shiny))
                                           (ray (vec 4 -1)
                                                (vec 0  1)))
      (values (tt in)
              (tt out))))

  (nst:def-test intersection-of-nested-spheres-is-inner-when-inner-second (:values (real= 3) (real= 9))
    (destructuring-bind ((in . out)) (hit* (csg-intersection
                                            (sphere (vec 4 5) 4 blue-shiny)
                                            (sphere (vec 4 5) 3 blue-diffuse))
                                           (ray (vec 4 -1)
                                                (vec 0  1)))
      (values (tt in)
              (tt out))))

  (nst:def-test intersection-of-overlapped-spheres-when-closer-first (:values (real= 6) (real= 9))
    (destructuring-bind ((in . out)) (hit* (csg-intersection
                                            (sphere (vec 4 5) 3 blue-diffuse)
                                            (sphere (vec 4 8) 3 blue-shiny))
                                           (ray (vec 4 -1)
                                                (vec 0  1)))
      (values (tt in)
              (tt out))))

  (nst:def-test intersection-of-overlapped-spheres-when-closer-second (:values (real= 6) (real= 9))
    (destructuring-bind ((in . out)) (hit* (csg-intersection
                                            (sphere (vec 4 8) 3 blue-shiny)
                                            (sphere (vec 4 5) 3 blue-diffuse))
                                           (ray (vec 4 -1)
                                                (vec 0  1)))
      (values (tt in)
              (tt out))))

  (nst:def-test intersection-of-three (:values (real= 9) (real= 9))
    (destructuring-bind ((in . out)) (hit* (csg-intersection
                                            (sphere (vec 4 5) 3 blue-diffuse)
                                            (sphere (vec 4 8) 3 blue-shiny)
                                            (sphere (vec 4 11) 3 blue-diffuse))
                                           (ray (vec 4 -1)
                                                (vec 0  1)))
      (values (tt in)
              (tt out))))


  (nst:def-test can-get-first-hit-from-an-intersection (real= 6)
    (tt (hit (csg-intersection
              (sphere (vec 4 5) 3 blue-diffuse)
              (sphere (vec 4 8) 3 blue-shiny))
             (ray (vec 4 -1)
                  (vec 0  1))
             (interval 0 10))))

  (nst:def-test can-get-second-hit-from-an-intersection (real= 9)
    (tt (hit (csg-intersection
              (sphere (vec 4 5) 3 blue-diffuse)
              (sphere (vec 4 8) 3 blue-shiny))
             (ray (vec 4 -1)
                  (vec 0  1))
             (interval 8 10))))

  )
