;;;; test/hit.lisp

(in-package #:weekend-raytracer/test)

(nst:def-test-group full-hit-construction-tests ()
  (nst:def-test can-construct-a-full-hit (:true)
    (full-hit 3 t (vec 1 2 3) (vec 1 0 0)))

  (nst:def-test constructor-raises-error-if-normal-not-unit-vector (:err)
    (full-hit 3 t (vec 1 2 3) (vec 1 1 0))))

(nst:def-test-group full-hit-accessor-tests ()
  (nst:def-test can-get-tt-from-full-hit (:equalp #.(vector-component 3))
    (tt (full-hit 3 t (vec 1 2 3) (vec 1 0 0))))

  (nst:def-test can-get-true-front-face-p-from-full-hit (:true)
    (and (front-face-p (full-hit 3 t (vec 1 2 3) (vec 1 0 0)))
         (not (front-face-p (full-hit 3 nil (vec 1 2 3) (vec 1 0 0))))))

  (nst:def-test can-get-point-from-full-hit (:equalp #.(vec 1 2 3))
    (point (full-hit 3 t (vec 1 2 3) (vec 1 0 0))))

  (nst:def-test can-get-normal-from-full-hit (:equalp 0)
    (vlen^2 (v- (normal (full-hit 3 t (vec 1 2 3) (vec 1 0 0)))
                (vec 1 0 0)))))

(nst:def-test-group partial-hit-construction-tests ()
  (nst:def-test can-construct-a-partial-hit (:true)
    (let ((tt 3))
      (flet ((thunk ()
               (full-hit tt t (vec 1 2 3) (vec 1 0 0))))
        (partial-hit tt #'thunk)))))

(nst:def-test-group partial-hit-accessor-tests ()
  (nst:def-test can-get-tt-from-partial-hit (:equalp #.(vector-component 3))
    (let ((tt 3))
      (flet ((thunk ()
               (full-hit tt t (vec 1 2 3) (vec 1 0 0))))
        (tt (partial-hit tt #'thunk)))))

  (nst:def-test can-convert-partial-hit-to-full-hit (:equalp #.(vec 1 2 3))
    (let ((tt 3))
      (flet ((thunk ()
               (full-hit tt t (vec 1 2 3) (vec 1 0 0))))
        (point (to-full-hit (partial-hit tt #'thunk))))))

  (nst:def-test to-full-hit-caches-result (:equalp 1)
    (let ((cntr 0)
          (tt 3))
      (flet ((thunk ()
               (incf cntr)
               (full-hit tt t (vec 1 2 3) (vec 1 0 0))))
        (let ((hit (partial-hit tt #'thunk)))
          (to-full-hit hit)
          (to-full-hit hit)
          cntr)))))

(nst:def-test-group hit-list-of-objects-tests ()
  (nst:def-test misses-empty-list (:equalp nil)
    (hit nil
         (ray (vec 0) (vec 1))
         (interval 0
                   10)))

  (nst:def-test can-hit-first-sphere (:equalp 4)
    (values (round (tt (hit (list (sphere (vec 5) 1)
                                  (sphere (vec 10) 1))
                            (ray (vec 0) (vec 1))
                            (interval 0
                                      10))))))

  (nst:def-test can-hit-second-sphere (:equalp 4)
    (values (round (tt (hit (list (sphere (vec 10) 1)
                                  (sphere (vec 5) 1))
                            (ray (vec 0) (vec 1))
                            (interval 0
                                      10)))))))
