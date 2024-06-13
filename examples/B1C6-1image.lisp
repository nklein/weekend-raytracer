;;;; examples/B1C6-1image.lisp
;;;;
;;;; This is the analog to Book 1, Chapter 6, Section 1's output image.

(in-package #:weekend-raytracer/examples)

(set-optimization-level)

(declaim (inline %hit-sphere-B1C6-1)
         (type (function (sphere ray) vector-component-type) %hit-sphere-B1C6-1))
(defun hit-sphere-B1C6-1 (sphere ray)
  (with-policy-expectations
      ((type sphere sphere)
       (type ray ray)
       (returns vector-component-type))
    (with-sphere (center radius radius^2) sphere
      (declare (ignore radius))
      (with-ray (origin direction) ray
        (let ((oc (v- center origin)))
          (let ((a (vlen^2 direction))
                (b (* #.(vector-component -2)
                      (v. direction oc)))
                (c (- (vlen^2 oc)
                      radius^2)))
            (let ((d (- (* b b)
                        (* #.(vector-component 4) a c))))
              (if (minusp d)
                  #.(vector-component -1)
                  (/ (- (- b)
                        (the vector-component-type (sqrt d)))
                     (* #.(vector-component 2) a))))))))))

(declaim (inline %ray-color-B1C6-1)
         (type (function (ray) color) %ray-color-B1C6-1))
(defun %ray-color-B1C6-1 (ray sphere)
  (with-policy-expectations
      ((type ray ray)
       (type sphere sphere)
       (returns color))
    (let ((tt (hit-sphere-B1C6-1 sphere ray)))
      (cond
        ((not (minusp tt))
         (let ((n (v/ (v- (at ray tt)
                          (center sphere))
                      (radius sphere))))
           (flet ((to-color (ii)
                    (/ (1+ (color-component (vref n ii)))
                       #.(color-component 2)))
                  (avg-color (a b)
                    (declare (type color-component-type a b))
                    (/ (+ a b)
                       #.(color-component 2))))
             (color (avg-color (to-color 1)
                               (to-color 1))
                    (to-color 2)
                    (to-color 0)))))
        (t
         (let* ((unit-direction (unit-vector (direction ray)))
                (a (/ (1+ (vref unit-direction 2)) 2))
                (b (/ (1+ (vref unit-direction 3)) 2)))
           (clerp (clerp #.(color 1 1 1)
                         #.(color 1/2 7/10 1)
                         a)
                  #.(color 1 7/10 1/2)
                  b)))))))

(defun b1c6-1image (&optional verticalp)
  "This example renders an image cube that is 320x180x5.

The optional parameter VERTICALP can be used to have the output image
oriented so that the depth slices go down the image rather than across
it.

The color is based upon the camera ray direction's 3rd and 4th
coordinates."

  (let* ((aspect-ratio 16/9)
         (width 320)
         (height (max (round (/ width aspect-ratio)) 1))
         (depth 5)
         (color-dimensions 3)
         (img (make-array (list width height depth color-dimensions)
                          :element-type 'color-component-type
                          :initial-element (color-component 0))))

    (let* ((focal-length 1.0d0)

           (viewport-height 2.0d0)
           (viewport-width (* viewport-height width (/ height)))
           (viewport-depth 1.5d0)

           (camera-center (vec 0 0 0 0))

           (viewport-u (vec 0 viewport-width 0 0))
           (viewport-v (vec 0 0 (- viewport-height) 0))
           (viewport-w (vec 0 0 0 viewport-depth))

           (pixel-delta-u (v/ viewport-u (vector-component width)))
           (pixel-delta-v (v/ viewport-v (vector-component height)))
           (pixel-delta-w (v/ viewport-w (vector-component depth)))

           (viewport-upper-left (reduce #'v-
                                        (list camera-center
                                              (vec focal-length 0 0 0)
                                              (v/ viewport-u 2)
                                              (v/ viewport-v 2)
                                              (v/ viewport-w 2))))
           (pixel000-loc (v+ viewport-upper-left
                             (v/ (reduce #'v+
                                         (list pixel-delta-u
                                               pixel-delta-v
                                               pixel-delta-w))
                                 2)))
           (sphere (sphere (vec -1 0 0 1/32) 1/2)))
      (loop :for z :below depth
            :for du := (v* pixel-delta-w z)
            :for z-loc := (v+ pixel000-loc du)
            :do (loop :for y :below height
                      :for dv := (v* pixel-delta-v y)
                      :for yz-loc := (v+ z-loc dv)
                      :do (loop :for x :below width
                                :for dw := (v* pixel-delta-u x)
                                :for xyz-loc := (v+ yz-loc dw)
                                :for pixel-center := xyz-loc
                                :for ray-direction := (v- pixel-center camera-center)
                                :for ray := (ray camera-center ray-direction)
                                :for pixel-color := (%ray-color-B1C6-1 ray sphere)
                                :do (setf (aref img x y z 0) (cref pixel-color 0)
                                          (aref img x y z 1) (cref pixel-color 1)
                                          (aref img x y z 2) (cref pixel-color 2))))))
    (write-image #P"B1C6-1image" img
                 :border-width 2
                 :border-color (vector 0 0 0 0)
                 :permutation (if verticalp
                                  (vector 0 1 2)
                                  (vector 0 2 1))
                 :cutoff (if verticalp
                             1
                             2))))
