;;;; examples/B1C12-19image.lisp

(in-package #:weekend-raytracer/examples)

(set-optimization-level)

(defun b1c12-19image (samples-per-pixel &optional verticalp)
  "This example renders an image cube that is 320x180x5.

The optional parameter VERTICALP can be used to have the output image
oriented so that the depth slices go down the image rather than across
it.

One of the spheres is lambertian, one metal, one dialectric with
index of refraction less than that of the air."

  (let* ((aspect-ratios '(1 16/9 64))
         (width 320)
         (rr (cos (/ pi 4)))
         (center (vec 0 0 0 0))
         (viewport '(32/9 2 3/2))
         (permutation (if verticalp
                          '(0 1 2)
                          '(0 2 1)))
         (spatial-dimensions 4)
         (color-dimensions 3)
         (camera (camera :width width
                         :aspect-ratios aspect-ratios
                         :viewport viewport
                         :center center
                         :spatial-dimensions spatial-dimensions
                         :color-dimensions color-dimensions
                         :max-depth 10
                         :field-of-view 90)))

    (let* ((world (list (sphere (vec -1 (- rr) 0 0) rr
                                (lambertian (color 0 0 1)))
                        (sphere (vec -1 (+ rr) 0 0) rr
                                (lambertian (color 1 0 0)))))
           (img (render camera world :samples-per-pixel samples-per-pixel)))
    (write-image #P"B1C12-19image" img
                 :border-width 2
                 :border-color (vector 0 0 0)
                 :permutation permutation
                 :cutoff (if verticalp
                             1
                             2)
                 :gamma 2.0d0))))