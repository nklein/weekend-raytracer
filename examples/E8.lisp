;;;; examples/E8.lisp

(in-package #:weekend-raytracer/examples)

(set-optimization-level)

(defun gen-e8-points ()
  (let ((res))
    (labels ((vec8 ()
               (list 0 0 0 0 0 0 0 0))
             (add (x)
               (setf res (list* (apply #'vec x) res)))
             (z8-from-indexes (indexes)
               (let ((vv (vec8)))
                 (destructuring-bind (aa bb) indexes
                   (flet ((add-it (xx yy)
                            (setf (elt vv aa) xx
                                  (elt vv bb) yy)
                            (add vv)))
                     (add-it  1  1)
                     (add-it  1 -1)
                     (add-it -1  1)
                     (add-it -1 -1)))))
             (z8+1/2-from-bitmask (mask)
               (when (evenp (logcount mask))
                 (loop :with vv := (vec8)
                       :for ii :below 8
                       :do (setf (elt vv ii) (if (zerop (ldb (byte 1 ii) mask))
                                                 1/2
                                                 -1/2))
                       :finally (add vv)))))
      (alexandria:map-combinations #'z8-from-indexes '(0 1 2 3 4 5 6 7) :length 2)
      (loop :for ii :below 256
            :do (z8+1/2-from-bitmask ii)))
    res))

(defun random-e8-albedo ()
  (apply #'color (loop :repeat 3
                       :collect (+ 0.5d0 (random 0.5d0)))))

(defun e8-image (samples-per-pixel)
  "This example renders the spheres in the E8 lattice
which kiss the sphere at the origin."

  (let* ((aspect-ratios '(1 1 48 48 48 48 48))
         (width 160)
         (center (vec 0 0 0 0 0 0 0 0))
         (lookat (vec 1 0 0 0 0 0 0 0))
         (orientation (list (vec 0 1 1 0 0 0 0 0)))
         (viewport '(1 1 1 1 1 1 1))
         (spatial-dimensions 8)
         (color-dimensions 3)
         (camera (camera :width width
                         :aspect-ratios aspect-ratios
                         :viewport viewport
                         :center center
                         :lookat lookat
                         :orientation orientation
                         :spatial-dimensions spatial-dimensions
                         :color-dimensions color-dimensions
                         :field-of-view 120
                         :focal-length (sqrt 2)
                         :focus-angle 0.6
                         :max-depth 20)))

    (let* ((world (loop :for vv :in (gen-e8-points)
                        :collecting (sphere vv (sqrt 1/2) (metal (random-e8-albedo) 0))))
           (img (render camera world :samples-per-pixel samples-per-pixel)))
      (write-image #P"E8" img
                   :border-width 3
                   :border-color (vector 1 1 1 0)
                   :permutation (list 0 2 4 6 1 3 5)
                   :gamma 2.0d0))))
