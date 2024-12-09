;;;; examples/Halfspace.lisp

(in-package #:weekend-raytracer/examples)

(set-optimization-level)

(defun halfspace-image (samples-per-pixel &optional verticalp)
  "This example renders an image cube looking at a hypercube made
by intersecting halfspaces."

  (let* ((aspect-ratios '(1 16/9 64))
         (width 640)
         (center (vec 13 3 2 1))
         (lookat (vec 0 0 0 0))
         (orientation (list (vec 0 1 0 0)
                            (vec 1 0 0 1)
                            (vec 1 1 1 0)))
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

    (let* ((world (list (halfspace (unit-vector (vec 0 0 -1 2)) -9/8 (metal (color 1/2 1 3/4) 0.3))
                        (csg-intersection
                         (halfspace (vec  1  0  0  0) 1 (lambertian (color 1/1 1/2 1/2)))
                         (halfspace (vec -1  0  0  0) 1 (lambertian (color 3/4 1/2 1/2)))
                         (halfspace (vec  0  1  0  0) 1 (dialectric (color 1 1 1) 3/2))
                         (halfspace (vec  0 -1  0  0) 1 (dialectric (color 3/4 3/4 3/4) 3/2))
                         (halfspace (vec  0  0  1  0) 1 (lambertian (color 1/1 3/4 1/2)))
                         (halfspace (vec  0  0 -1  0) 1 (lambertian (color 1/2 3/4 1/2)))
                         (halfspace (vec  0  0  0  1) 1 (lambertian (color 1/1 1/2 3/4)))
                         (halfspace (vec  0  0  0 -1) 1 (lambertian (color 1/2 1/2 3/4))))))
           (img (render camera world :samples-per-pixel samples-per-pixel)))
      (write-image #P"Halfspace" img
                   :border-width 2
                   :border-color (vector 0 0 0)
                   :permutation permutation
                   :cutoff (if verticalp
                               1
                               2)
                   :gamma 2.0d0))))
