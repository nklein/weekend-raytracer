;;;; examples/Kiss3.lisp

(in-package #:weekend-raytracer/examples)

(set-optimization-level)

(defun kiss3-image (samples-per-pixel)
  "This renders a sphere being kissed by 12 other spheres in an icosohedron configuration."

  (let* ((aspect-ratios '(1 16/9))
         (width 1024)
         (origin (vec 0 0 0))
         (center (vec -20 10 12))
         (lookat origin)
         (orientation (list (vec 0 1 0)))
         (viewport '(32/9 2))
         (spatial-dimensions 3)
         (color-dimensions 3)
         (camera (camera :width width
                         :aspect-ratios aspect-ratios
                         :viewport viewport
                         :center center
                         :lookat lookat
                         :orientation orientation
                         :spatial-dimensions spatial-dimensions
                         :color-dimensions color-dimensions
                         :field-of-view 10
                         :focal-length (vlen center)
                         :focus-angle 0.6
                         :max-depth 20))
         (phi (/ (1+ (sqrt 5.0d0)) 2)))
    (flet ((kisser (x y z)
             (sphere (vec x y z)
                     1
                     (metal (color 3/4 3/4 4/4) 0.8))))

      (let* ((world (list (sphere origin 1 (lambertian (color 3/4 3/4 0/4)))
                          (kisser phi 1 0)
                          (kisser phi -1 0)
                          (kisser (- phi) 1 0)
                          (kisser (- phi) -1 0)
                          (kisser 0 phi 1)
                          (kisser 0 phi -1)
                          (kisser 0 (- phi) 1)
                          (kisser 0 (- phi) -1)
                          (kisser 1 0 phi)
                          (kisser -1 0 phi)
                          (kisser 1 0 (- phi))
                          (kisser -1 0 (- phi))
                          (halfspace (unit-vector (vec -10 20 100))
                                     (- (1+ phi))
                                     (metal (color 1/4 4/4 1/4) 0.2))))
             (img (render camera world :samples-per-pixel samples-per-pixel)))
        (write-image #P"Kiss3" img
                     :border-width 3
                     :border-color (vector 1 1 1 1)
                     :permutation '(0 1)
                     :cutoff 1
                     :gamma 2.0d0)))))
