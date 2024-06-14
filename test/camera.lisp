;;;; test/camera.lisp

(in-package #:weekend-raytracer/test)

(nst:def-test-group camera-construction-tests ()
  (nst:def-test can-construct-a-camera (:true)
    (camera :width 100
            :spatial-dimensions 4
            :color-dimensions 3))

  (nst:def-test can-construct-a-camera-with-aspect-ratios (:true)
    (camera :width 100
            :aspect-ratios (list 1 1/2 1/20)
            :spatial-dimensions 4
            :color-dimensions 3))

  (nst:def-test can-construct-a-camera-with-permutation (:true)
    (camera :width 100
            :permutation (list 0 2 1)
            :spatial-dimensions 4
            :color-dimensions 3))

  (nst:def-test constructor-raises-error-if-no-width-specified (:err)
    (camera :spatial-dimensions 4
            :color-dimensions 3))

  (nst:def-test constructor-raises-error-if-no-spatial-dimensions-specified (:err)
    (camera :width 100
            :color-dimensions 3))

  (nst:def-test constructor-raises-error-if-no-color-dimensions-specified (:err)
    (camera :width 100
            :spatial-dimensions 4))

  (nst:def-test constructor-raises-error-if-aspect-ratios-is-wrong-length (:err)
    (camera :width 100
            :aspect-ratios (list 1 1/2)
            :spatial-dimensions 4
            :color-dimensions 3))

  (nst:def-test constructor-raises-error-if-permutation-is-wrong-length (:err)
    (camera :width 100
            :permutation (list 0 1)
            :spatial-dimensions 4
            :color-dimensions 3))

  (nst:def-test constructor-raises-error-if-permutation-has-repeats (:err)
    (camera :width 100
            :permutation (list 0 1 0)
            :spatial-dimensions 4
            :color-dimensions 3))

  (nst:def-test constructor-raises-error-if-permutation-out-of-range (:err)
    (camera :width 100
            :permutation (list 1 2 3)
            :spatial-dimensions 4
            :color-dimensions 3))
  )
