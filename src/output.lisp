;;;; output.lisp

(in-package #:weekend-raytracer)

(defun %valid-permutation-p (permutation length)
  (and (every #'integerp permutation)
       (every (lambda (x)
                (<= 0 x (1- length)))
              permutation)
       (= (length permutation) length)
       (equalp permutation (remove-duplicates permutation :test #'=))))

(defun %collect-width-and-height-components (dimensions permutation cutoff)
  (let ((width nil)
        (height nil))
    (loop :for k :below (length dimensions)
          :for i := (nth k permutation)
          :do (if (< k cutoff)
                  (setf width (list* (nth i dimensions) width))
                  (setf height (list* (nth i dimensions) height))))
    (values (nreverse width)
            (nreverse height))))

(defun %border-count (ws)
  (flet ((product (l)
           (reduce #'* l)))
    (reduce #'+ (maplist #'product ws)
            :key #'1-)))

(defun %calculate-one-output-size (ws border-width)
  (let ((borderless-size (reduce #'* ws :initial-value 1))
        (border-count (%border-count (rest ws))))
    (+ borderless-size
       (* border-count
          border-width))))

(defun %calculate-output-size (dimensions border-width permutation cutoff)
  (multiple-value-bind (widths heights) (%collect-width-and-height-components dimensions permutation cutoff)
    (values (or (%calculate-one-output-size widths border-width)
                1)
            (or (%calculate-one-output-size heights border-width)
                1))))

(defun %border-p (xs ws)
  (not (every #'< xs ws)))

(defun %increment (xs ws border-width)
  (labels ((rec (xs ws)
             (when xs
               (incf (first xs))
               (cond
                 ((<= (+ (first ws) border-width) (first xs))
                  (setf (first xs) 0)
                  (rec (rest xs) (rest ws)))))))
    (rec xs ws)
    xs))

(defun %output-pixels (array &key
                               dimensions
                               color-dimensions
                               permutation
                               cutoff
                               width
                               height
                               border-width
                               border-color
                               verbose
                               output-pixel-fn)
  (multiple-value-bind (widths heights) (%collect-width-and-height-components dimensions permutation cutoff)
    (let ((pixel-indexes (mapcar (constantly 0) (list* 1 dimensions))))
      (flet ((map-indexes (is offset)
               (loop :for i :in is
                     :for k :from offset
                     :do (setf (nth (nth k permutation) pixel-indexes) i)))
             (output-border-pixel ()
               (funcall output-pixel-fn border-color))
             (output-pixel ()
               (funcall output-pixel-fn (loop :for k :below color-dimensions
                                              :collecting (progn
                                                            (setf (nth (length dimensions) pixel-indexes) k)
                                                            (apply #'aref array pixel-indexes))))))
        (loop :with ys := (mapcar (constantly 0) heights)
              :for y :below height
              :for border-row-p := (%border-p ys heights)
              :do (when verbose
                    (format *debug-io* "Scanline ~D of ~D~C" (1+ y) height #\Return))
              :finally (when verbose
                         (format *debug-io* "Done====================================~%"))
              :do (unless border-row-p
                    (map-indexes ys (length widths)))
              :do (unwind-protect
                       (loop :with xs := (mapcar (constantly 0) widths)
                             :for x :below width
                             :for border-column-p := (or border-row-p
                                                         (%border-p xs widths))
                             :do (unless border-column-p
                                   (map-indexes xs 0))
                             :do (unwind-protect
                                      (if border-column-p
                                          (output-border-pixel)
                                          (output-pixel))
                                   (%increment xs widths border-width)))
                    (%increment ys heights border-width)))))))

(defun %write-png-image (array filename
                         &key
                           dimensions
                           color-dimensions
                           output-color-dimensions
                           permutation
                           cutoff
                           width
                           height
                           border-width
                           border-color
                           verbose)
  (let ((png (make-instance 'zpng:pixel-streamed-png
                            :color-type (ecase output-color-dimensions
                                          (1 :grayscale)
                                          (2 :grayscale-alpha)
                                          (3 :truecolor)
                                          (4 :truecolor-alpha))
                            :width width
                            :height height)))
    (labels ((clamp (n)
               (let ((v (round (* 255.0d0 n))))
                 (cond
                   ((< v 0) 0)
                   ((< 255 v) 255)
                   (t v))))
             (resize-pixel (pixel)
               (let ((len (length pixel)))
                 (cond
                   ((= len output-color-dimensions)
                    pixel)
                   ((< len output-color-dimensions)
                    (append pixel (loop :repeat (- output-color-dimensions len) :collecting 1.0d0))))))
             (clamp-pixel (pixel)
               (mapcar #'clamp (resize-pixel pixel)))
             (output-pixel (pixel)
               (zpng:write-pixel (clamp-pixel pixel) png)))
      (with-open-file (stream (merge-pathnames filename
                                               (make-pathname :type "png"))
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
        (zpng:start-png png stream)
        (unwind-protect
             (%output-pixels array
                             :dimensions dimensions
                             :color-dimensions color-dimensions
                             :permutation permutation
                             :cutoff cutoff
                             :width width
                             :height height
                             :border-width border-width
                             :border-color border-color
                             :verbose verbose
                             :output-pixel-fn #'output-pixel)
          (zpng:finish-png png))))))

(defvar *verbose* nil
  "The default value for the VERBOSE flag to #'WRITE-IMAGE.")

(defun write-image (filename array
                    &key
                      (border-width 1)
                      (border-color nil)
                      (permutation nil)
                      (cutoff nil)
                      (verbose *verbose*))
  "This function takes a FILENAME in which to write the image and an
ARRAY of image pixel values.  The array can be any number of
dimensions. The final dimension must either by 1, 2, 3, or 4 however.
The output colorspace is determined by the size of the final
dimension:

    1 => grayscale
    2 => grayscale + alpha
    3 => RGB
    4 => RGBA

As the image cube may be more than two dimensions, the BORDER-WIDTH is
the thickness of the border between slices in the output image.  The
BORDER-COLOR is the pixel value to use for the borders between slices.
Note: if the image cube is grayscale or RGB and the BORDER-COLOR is
one longer than it would need to be, the image is promoted to include
an alpha channel.

The PERMUTATION specifies how to reorder the indices of the color cube
before output. To leave the image cube in the order it is, one can
leave this NIL or set it to (0 1 2 3 ...). The first CUTOFF permuted
indices contribute to the width of the output image while the
remaining pemuted indices contribute to teh height of the output
image.

If VERBOSE is non-NIL, the writing will output progress information.
The default value is taken from the *VERBOSE* special variable."
  (let* ((dimensions (array-dimensions array))
         (color-dimensions (first (last dimensions)))
         (dimensions (butlast dimensions))
         (permutation (or permutation
                          (loop :for i :below (length dimensions) :collecting i)))
         (cutoff (or cutoff
                     (ceiling (length permutation) 2)))
         (output-color-dimensions (length border-color))
         (border-color (or border-color
                           (loop :repeat color-dimensions :collecting 0))))
    (check-type border-width (integer 0))
    (assert (%valid-permutation-p permutation (length dimensions)) (permutation)
            "Permutation ~A does not word for dimensions ~D"
            permutation
            (length dimensions))
    (assert (or (null border-color)
                (zerop border-width)
                (= color-dimensions output-color-dimensions)
                (= (1+ color-dimensions) output-color-dimensions))
            (border-color)
            "Border-color not compatible with image color ~D dimensions in border-color while ~D in image color"
            (length border-color)
            color-dimensions)
    (multiple-value-bind (width height) (%calculate-output-size dimensions border-width permutation cutoff)
      (%write-png-image array filename
                        :dimensions dimensions
                        :color-dimensions color-dimensions
                        :output-color-dimensions output-color-dimensions
                        :permutation permutation
                        :cutoff cutoff
                        :width width
                        :height height
                        :border-width border-width
                        :border-color border-color
                        :verbose verbose))))
