;;;; examples/B1C14-23image.lisp

(in-package #:weekend-raytracer/examples)

(set-optimization-level)

(defun v (x y z w)
  (vec x z y w))

(defun 3v (a b c)
  (v a 2/10 b c))

(defun random-lambertian-albedo ()
  (apply #'color (loop :repeat 3
                       :collect (* (random 1.0d0)
                                   (random 1.0d0)))))
(defun random-metal-albedo ()
  (apply #'color (loop :repeat 3
                       :collect (+ 0.5d0 (random 0.5d0)))))

(defun make-random-sphere (cur maxes)
  (let ((pos (apply #'3v (loop :for cc :in cur
                               :for mm :in maxes
                               :collecting (+ (- cc (/ mm 2))
                                              (- (random 1.8d0)
                                                 0.9d0))))))
    (when (< 0.9 (vlen (v- pos (3v 4 0 0))))
      (let ((rr (random 1.0d0)))
        (cond
          ((< rr 0.8)
           (sphere pos 2/10 (lambertian (random-lambertian-albedo))))
          ((< rr 0.95)
           (sphere pos 2/10 (metal (random-metal-albedo) (+ 0.5d0 (random 0.5d0)))))
          (t
           (sphere pos 2/10 (dialectric (color 1 1 1) 1.5))))))))

(defun make-random-spheres ()
  (loop :with MAXES := (list 11 11 3)
        :for ii := (list 0 0 0) :then (increment-indexes ii MAXES)
        :while ii
        :for ss := (make-random-sphere ii MAXES)
        :when ss
          :collect ss))

(defun b1c14-23image (samples-per-pixel &optional verticalp)
  "This example renders a large image cube of a scene with some
defined large spheres and some random smaller spheres scattered
around them.

The optional parameter VERTICALP can be used to have the output image
oriented so that the depth slices go down the image rather than across
it."

  (let* ((aspect-ratios '(1 16/9 64))
         (width 640)
         (center (v 13 2 3 0))
         (lookat (v 0 1/2 0 0))
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

    (let* ((world (list* (sphere (v 0 -1000 0 0) 1000
                                 (lambertian (color 1/2 1/2 1/2)))
                         (sphere (v 0 1 0 0) 1
                                 (dialectric (color 1 1 1) 3/2))
                         (sphere (v -4 1 0 0) 1
                                 (lambertian (color 4/10 2/10 1/10)))
                         (sphere (v 4 1 0 0) 1
                                 (metal (color 7/10 6/10 5/10) 0.0))
                         (make-random-spheres)))
           (img (render camera world :samples-per-pixel samples-per-pixel)))
    (write-image #P"B1C14-23image" img
                 :border-width 2
                 :border-color (vector 0 0 0)
                 :permutation permutation
                 :cutoff (if verticalp
                             1
                             2)
                 :gamma 2.0d0))))
