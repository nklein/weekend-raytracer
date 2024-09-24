;;;; examples/B1C10-13image.lisp

(in-package #:weekend-raytracer/examples)

(set-optimization-level)

(defun b1c10-13image (samples-per-pixel &optional verticalp)
  "This example renders an image cube that is 320x180x5.

The optional parameter VERTICALP can be used to have the output image
oriented so that the depth slices go down the image rather than across
it.

The color is true Lambertian scattering off of gray spheres with gamma correction."

  (let* ((aspect-ratios '(1 16/9 64))
         (width 320)
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
                         :max-depth 10)))

    (let* ((world (list (sphere (vec -1 0 0 1/32) 1/2
                                (lambertian (color 1/10 2/10 5/10)))
                        (sphere (vec -1 -1 0 0) 1/2
                                (metal (color 8/10 8/10 8/10)))
                        (sphere (vec -1  1 0  5/16) 1/2
                                (metal (color 8/10 6/10 2/10)))
                        (sphere (vec -1 0 -100.5 -1) 100
                                (lambertian (color 8/10 8/10 0/10)))))
           (img (render camera world :samples-per-pixel samples-per-pixel)))
    (write-image #P"B1C10-13image" img
                 :border-width 2
                 :border-color (vector 0 0 0)
                 :permutation permutation
                 :cutoff (if verticalp
                             1
                             2)
                 :gamma 2.0d0))))