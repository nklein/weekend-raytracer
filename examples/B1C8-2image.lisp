;;;; examples/B1C8-2image.lisp
;;;;
;;;; This is similar to B1C6-7 but is using the camera class now.

(in-package #:weekend-raytracer/examples)

(set-optimization-level)

(defun b1c8-2image (samples-per-pixel &optional verticalp)
  "This example renders an image cube that is 320x180x5.

The optional parameter VERTICALP can be used to have the output image
oriented so that the depth slices go down the image rather than across
it.

The color is based upon the camera ray direction's 3rd and 4th
coordinates."

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
                         :color-dimensions color-dimensions)))

    (let* ((world (list (sphere (vec 1 0 0 1/32) 1/2 (lambertian (color 1/2 1/2 1/2)))
                        (sphere (vec 1 0 -100.5 -1) 100 (lambertian (color 1/2 1/2 1/2)))))
           (img (render camera world :samples-per-pixel samples-per-pixel)))
    (write-image #P"B1C8-2image" img
                 :border-width 2
                 :border-color (vector 0 0 0)
                 :permutation permutation
                 :cutoff (if verticalp
                             1
                             2)))))
