;;;; examples/Kiss4.lisp

(in-package #:weekend-raytracer/examples)

(set-optimization-level)

(defun kiss4-image (samples-per-pixel &optional verticalp)
  "This renders a 4D ball being kissed by 24 other balls in a 16-cell honeycomb."

  (let* ((aspect-ratios '(1 16/9 64))
         (width 640)
         (origin (vec 0 0 0 0))
         (center (vec -20 10 12 0))
         (lookat origin)
         (orientation (list (vec 0 1 0 0)))
         (viewport '(32/9 2 3/2))
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
                         :field-of-view 12.5
                         :focal-length (vlen center)
                         :focus-angle 0.6
                         :max-depth 20)))
    (labels ((random-albedo ()
               (apply #'color (loop :repeat 3
                                    :collect (* (random 1.0d0)
                                                (random 1.0d0)))))
             (kisser (x y z w)
               (sphere (vec x y z w)
                       1
                       (metal (random-albedo) 0.8))))

      (let* ((world (list (sphere origin 1 (metal (color 1 1 1) 0))
                          (kisser +2 0 0 0)
                          (kisser -2 0 0 0)
                          (kisser 0 +2 0 0)
                          (kisser 0 -2 0 0)
                          (kisser 0 0 +2 0)
                          (kisser 0 0 -2 0)
                          (kisser 0 0 0 +2)
                          (kisser 0 0 0 -2)
                          (kisser +1 +1 +1 +1)
                          (kisser +1 +1 +1 -1)
                          (kisser +1 +1 -1 +1)
                          (kisser +1 +1 -1 -1)
                          (kisser +1 -1 +1 +1)
                          (kisser +1 -1 +1 -1)
                          (kisser +1 -1 -1 +1)
                          (kisser +1 -1 -1 -1)
                          (kisser -1 +1 +1 +1)
                          (kisser -1 +1 +1 -1)
                          (kisser -1 +1 -1 +1)
                          (kisser -1 +1 -1 -1)
                          (kisser -1 -1 +1 +1)
                          (kisser -1 -1 +1 -1)
                          (kisser -1 -1 -1 +1)
                          (kisser -1 -1 -1 -1)
                          (halfspace (unit-vector (vec -10 20 180 5))
                                     -3
                                     (metal (color 1/4 4/4 1/4) 0.2))))
             (img (render camera world :samples-per-pixel samples-per-pixel)))
        (write-image #P"Kiss4" img
                     :border-width 3
                     :border-color (vector 1 1 1 1)
                     :permutation '(0 2 1)
                     :cutoff (if verticalp
                                 1
                                 2)
                     :gamma 2.0d0)))))
