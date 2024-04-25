;;;; examples/B1C2-2image.lisp
;;;;
;;;; This is the analog to Book 1, Chapter 2, Section 2's output image.

(in-package #:weekend-raytracer/examples)

(defun b1c2-2image (&optional verticalp)
  "This example renders an image cube that is 256x256x5.  The first axis
increases in redness, the second axis increases in greenness, and the
third axis increase in blueness.

The optional parameter VERTICALP can be used to have the output image
oriented so that the depth slices go down the image rather than across
it."

  (let* ((width 256)
         (height 256)
         (depth 5)
         (color-dimensions 3)
         (img (make-array (list width height depth color-dimensions)
                          :element-type 'color-component-type
                          :initial-element (color-component 0))))
    (loop :for z :below depth
          :for blue := (color-component (/ z (1- depth)))
          :do (loop :for y :below height
                    :for green := (color-component (/ y (1- height)))
                    :do (loop :for x :below width
                              :for red := (color-component (/ x (1- width)))
                              :do (setf (aref img x y z 0) red
                                        (aref img x y z 1) green
                                        (aref img x y z 2) blue))))
    (write-image #P"B1C2-2image" img
                 :border-width 2
                 :border-color (vector 0 0 0 0)
                 :permutation (if verticalp
                                  (vector 0 1 2)
                                  (vector 0 2 1))
                 :cutoff (if verticalp
                             1
                             2))))
