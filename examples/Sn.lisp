;;;; examples/Sn.lisp

(in-package #:weekend-raytracer/examples)

(set-optimization-level)

(defun sn-image (n samples-per-pixel &optional verticalp)
  "This example renders the an image of S^n."

  (let* ((n+1 (1+ n))
         (aspect-ratios (subseq '(1 16/9 128 128 128 128 128 128) 0 n))
         (width 640)
         (origin (apply #'vec (subseq '(0 0 0 0 0 0 0 0) 0 n+1)))
         (center (apply #'vec (subseq '(-4 0 1/2 0 0 0 0 0) 0 n+1)))
         (lookat origin)
         (orientation (list (apply #'vec (subseq '(0 1 0 0 0 0 0 0) 0 n+1))))
         (viewport (subseq '(32/9 2 3/2 3/2 3/2 3/2 3/2 3/2) 0 n))
         (spatial-dimensions n+1)
         (color-dimensions 3)
         (camera (camera :width width
                         :aspect-ratios aspect-ratios
                         :viewport viewport
                         :center center
                         :lookat lookat
                         :orientation orientation
                         :spatial-dimensions spatial-dimensions
                         :color-dimensions color-dimensions
                         :field-of-view 40
                         :focal-length (vlen center)
                         :focus-angle 0.6
                         :max-depth 20)))

    (let* ((world (list (sphere origin 1 (lambertian (color 4/4 1/4 1/4)))
                        (when (< 2 n)
                          (halfspace (apply #'vec (subseq '(0 0 1 0 0 0 0 0 0) 0 n))
                                     -1
                                     (metal (color 1/4 4/4 1/4) 0.2)))))
           (img (render camera world :samples-per-pixel samples-per-pixel)))
      (write-image (make-pathname :name (format nil "Sn-~A" n)) img
                   :border-width 3
                   :border-color (vector 1 1 1 1)
                   :permutation (case n
                                  (1 '(0))
                                  (2 '(0 1))
                                  (3 '(0 2 1))
                                  (4 '(0 2 1 3)))
                   :cutoff (if verticalp
                               (floor (1- n) 2)
                               (1+ (floor (1- n) 2)))
                   :gamma 2.0d0))))
