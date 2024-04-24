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
         (element-type 'double-float)
         (img (make-array (list width height depth color-dimensions)
                          :element-type element-type
                          :initial-element (coerce 0 element-type))))
    (loop :for z :below depth
          :for blue := (coerce (/ z (1- depth)) element-type)
          :do (loop :for y :below height
                    :for green := (coerce (/ y (1- height)) element-type)
                    :do (loop :for x :below width
                              :for red := (coerce (/ x (1- width)) element-type)
                              :do (setf (aref img x y z 0) red
                                        (aref img x y z 1) green
                                        (aref img x y z 2) blue))))
    (write-image #P"B1C2-2image" img
                 :border-width 2
                 :border-color '(0 0 0 0)
                 :permutation (if verticalp
                                  '(0 1 2)
                                  '(0 2 1))
                 :cutoff (if verticalp
                             1
                             2))))
