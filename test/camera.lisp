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

  (nst:def-test can-construct-a-camera-with-viewport (:true)
    (camera :width 100
            :viewport (list 2 2 3/2)
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

  (nst:def-test constructor-raises-error-if-viewport-is-wrong-length (:err)
    (camera :width 100
            :viewport (list 2 3)
            :spatial-dimensions 4
            :color-dimensions 3)))
