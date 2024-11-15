;;;; examples/CSG-Intersect.lisp

(in-package #:weekend-raytracer/examples)

(set-optimization-level)

(defun csg-intersect-image (samples-per-pixel &optional verticalp)
  "This example renders an image cube looking at the intersection
of three spheres."

  (let* ((aspect-ratios '(1 16/9 64))
         (width 640)
         (center (vec 13 3 2 0))
         (lookat (vec 0 0 1/2 3/4))
         (orientation (list (vec 0 1 0 0)))
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
                         :field-of-view 15
                         :focal-length 10.0
                         :focus-angle 0.6
                         :max-depth 20)))

    (let* ((world (list (sphere (vec 0 0 -1000 0) 999
                                (lambertian (color 1/2 1/2 3/4)))
                        (sphere (vec -3 -2 0 7/8) 2
                                (metal (color 3/4 1/4 1/4) 0.0))
                        (sphere (vec -5 2 0 1) 2
                                (metal (color 1/4 3/4 1/4) 0.0))
                        (csg-intersection
                         (sphere (vec 0 2 0 1) 5/2
                                 (dialectric (color 1 1 1) 3/2))
                         (sphere (vec 0 1 0 0) 2
                                 (lambertian (color 4/10 2/10 1/10)))
                         (sphere (vec 0 0 1 1) 2
                                 (metal (color 9/10 8/10 3/10) 0.0)))))
           (img (render camera world :samples-per-pixel samples-per-pixel)))
    (write-image #P"CSG-Intersect" img
                 :border-width 2
                 :border-color (vector 0 0 0)
                 :permutation permutation
                 :cutoff (if verticalp
                             1
                             2)
                 :gamma 2.0d0))))
