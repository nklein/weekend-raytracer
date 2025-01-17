;;;; test/hit.lisp

(in-package #:weekend-raytracer/test)

(nst:def-test-group full-hit-construction-tests (materials-fixture)
  (nst:def-test can-construct-a-full-hit (:true)
    (full-hit 3 t (vec 1 2 3) (vec 1 0 0) blue-diffuse))

  (nst:def-test constructor-raises-error-if-normal-not-unit-vector (:err)
    (full-hit 3 t (vec 1 2 3) (vec 1 1 0) blue-diffuse)))

(nst:def-test-group full-hit-accessor-tests (materials-fixture)
  (nst:def-test can-get-tt-from-full-hit (:equalp #.(vector-component 3))
    (tt (full-hit 3 t (vec 1 2 3) (vec 1 0 0) blue-diffuse)))

  (nst:def-test can-get-true-front-face-p-from-full-hit (:true)
    (and (front-face-p (full-hit 3 t (vec 1 2 3) (vec 1 0 0) blue-diffuse))
         (not (front-face-p (full-hit 3 nil (vec 1 2 3) (vec 1 0 0) blue-diffuse)))))

  (nst:def-test can-get-point-from-full-hit (vec= #.(vec 1 2 3))
    (point (full-hit 3 t (vec 1 2 3) (vec 1 0 0) blue-diffuse)))

  (nst:def-test can-get-normal-from-full-hit (:equalp 0)
    (vlen^2 (v- (normal (full-hit 3 t (vec 1 2 3) (vec 1 0 0) blue-diffuse))
                (vec 1 0 0))))

  (nst:def-test can-get-material-from-full-hit (:eql blue-diffuse)
    (hit-material (full-hit 3 t (vec 1 2 3) (vec 1 0 0) blue-diffuse))))

(nst:def-test-group partial-hit-construction-tests (materials-fixture)
  (nst:def-test can-construct-a-partial-hit (:true)
    (let ((tt 3))
      (flet ((thunk ()
               (full-hit tt t (vec 1 2 3) (vec 1 0 0) blue-diffuse)))
        (partial-hit tt t #'thunk)))))

(nst:def-test-group partial-hit-accessor-tests (materials-fixture)
  (nst:def-test can-get-tt-from-partial-hit (:equalp #.(vector-component 3))
    (let ((tt 3))
      (flet ((thunk ()
               (full-hit tt t (vec 1 2 3) (vec 1 0 0) blue-diffuse)))
        (tt (partial-hit tt t #'thunk)))))

  (nst:def-test can-get-front-face-p-from-partial-hit (:values :true (:not :true))
    (let ((tt 3))
      (flet ((thunk ()
               (full-hit tt t (vec 1 2 3) (vec 1 0 0) blue-diffuse)))
        (values (front-face-p (partial-hit tt t #'thunk))
                (front-face-p (partial-hit tt nil #'thunk))))))

  (nst:def-test can-convert-partial-hit-to-full-hit (:equalp #.(vec 1 2 3))
    (let ((tt 3))
      (flet ((thunk ()
               (full-hit tt t (vec 1 2 3) (vec 1 0 0) blue-diffuse)))
        (point (to-full-hit (partial-hit tt t #'thunk))))))

  (nst:def-test to-full-hit-caches-result (:equalp 1)
    (let ((cntr 0)
          (tt 3))
      (flet ((thunk ()
               (incf cntr)
               (full-hit tt t (vec 1 2 3) (vec 1 0 0) blue-diffuse)))
        (let ((hit (partial-hit tt t #'thunk)))
          (to-full-hit hit)
          (to-full-hit hit)
          cntr)))))

(nst:def-test-group hit-list-of-objects-tests (materials-fixture)
  (nst:def-test misses-empty-list (:equalp nil)
    (hit nil
         (ray (vec 0) (vec 1))
         (interval 0
                   10)))

  (nst:def-test can-hit-first-sphere (:equalp 4)
    (values (round (tt (hit (list (sphere (vec 5) 1 blue-diffuse)
                                  (sphere (vec 10) 1 blue-diffuse))
                            (ray (vec 0) (vec 1))
                            (interval 0
                                      10))))))

  (nst:def-test can-hit-second-sphere (:equalp 4)
    (values (round (tt (hit (list (sphere (vec 10) 1 blue-diffuse)
                                  (sphere (vec 5) 1 blue-diffuse))
                            (ray (vec 0) (vec 1))
                            (interval 0
                                      10)))))))
