;;;; examples/B1C12-20image.lisp

(in-package #:weekend-raytracer/examples)

(set-optimization-level)

(defun b1c12-20image (samples-per-pixel &optional verticalp)
  "This example renders an image cube that is 320x180x5.

The optional parameter VERTICALP can be used to have the output image
oriented so that the depth slices go down the image rather than across
it.

One of the spheres is lambertian, one metal, one dialectric with
index of refraction less than that of the air."

  (let* ((aspect-ratios '(1 16/9 64))
         (width 320)
         (center (vec 1 -2 2 0))
         (lookat (vec -1 0 0 1/32))
         (orientation (list (vec 1 1 0 0)))
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
                         :lookat lookat
                         :orientation orientation
                         :spatial-dimensions spatial-dimensions
                         :color-dimensions color-dimensions
                         :field-of-view 90
                         :max-depth 10)))

    (let* ((world (list (sphere (vec -1 0 0 1/32) 1/2
                                (lambertian (color 1/10 2/10 5/10)))
                        (sphere (vec -1 -1 0 0) 1/2
                                (dialectric (color 1 1 1) 3/2))
                        (sphere (vec -1 -1 0 0) 2/5
                                (dialectric (color 1 1 1) 2/3))
                        (sphere (vec -1  1 0  5/16) 1/2
                                (metal (color 8/10 6/10 2/10) 1))
                        (sphere (vec -1 0 -100.5 -1) 100
                                (lambertian (color 8/10 8/10 0/10)))))
           (img (render camera world :samples-per-pixel samples-per-pixel)))
    (write-image #P"B1C12-20image" img
                 :border-width 2
                 :border-color (vector 0 0 0)
                 :permutation permutation
                 :cutoff (if verticalp
                             1
                             2)
                 :gamma 2.0d0))))
