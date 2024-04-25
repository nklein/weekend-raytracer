;;;; output.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(deftype index-vector () '(vector t))

(defun %valid-permutation-p (permutation length)
  (and (every #'integerp permutation)
       (every (lambda (x)
                (<= 0 x (1- length)))
              permutation)
       (= (length permutation) length)
       (equalp permutation (remove-duplicates permutation :test #'=))))

(defun %collect-width-and-height-components (dimensions permutation cutoff)
  (with-policy-expectations
      ((type (or null index-vector) dimensions permutation)
       (type fixnum cutoff)
       (assertion (%valid-permutation-p permutation (length dimensions))))
    (let ((width nil)
          (height nil))
      (with-policy-expectations
          ((type list width height))
        (loop :for k :below (length dimensions)
              :for i := (elt permutation k)
              :do (if (< k cutoff)
                      (setf width (list* (elt dimensions i) width))
                      (setf height (list* (elt dimensions i) height))))
        (values (coerce (nreverse width) 'index-vector)
                (coerce (nreverse height) 'index-vector))))))

(defun %border-count (ws)
  (with-policy-expectations
      ((type index-vector ws)
       (returns fixnum))
    (flet ((product (l)
             (with-policy-expectations
                 ((type list l)
                  (returns fixnum))
               (reduce #'* l))))
      (reduce #'+ (maplist #'product (coerce ws 'list))
              :key #'1-))))

(defun %calculate-one-output-size (ws border-width)
  (with-policy-expectations
      ((type index-vector ws)
       (type fixnum border-width)
       (returns fixnum))
    (let ((borderless-size (reduce #'* ws :initial-value 1))
          (border-count (if (and ws (plusp (length ws)))
                            (%border-count (subseq ws 1))
                            0)))
      (with-policy-expectations
          ((type fixnum borderless-size border-count))
        (+ borderless-size
           (* border-count
              border-width))))))

(defun %calculate-output-size (dimensions border-width permutation cutoff)
  (with-policy-expectations
      ((type (or null index-vector) dimensions permutation)
       (type fixnum border-width cutoff)
       (assertion (%valid-permutation-p permutation (length dimensions))))
    (multiple-value-bind (widths heights) (%collect-width-and-height-components dimensions permutation cutoff)
      (values (with-policy-expectations
                  ((returns fixnum))
                (or (%calculate-one-output-size widths border-width)
                    1))
              (with-policy-expectations
                  ((returns fixnum))
                (or (%calculate-one-output-size heights border-width)
                    1))))))

(defun %border-p (xs ws)
  (with-policy-expectations
      ((type index-vector xs ws)
       (returns boolean))
    (not (every #'< xs ws))))

(defun %increment (xs ws border-width)
  (with-policy-expectations
      ((type index-vector xs ws)
       (type fixnum border-width)
       (returns index-vector))
    (labels ((rec (xs ws index)
               (with-policy-expectations
                   ((type index-vector xs ws)
                    (type fixnum index))
                 (when xs
                   (incf (elt xs index))
                   (cond
                     ((<= (+ (elt ws index) border-width) (elt xs index))
                      (setf (elt xs index) 0)
                      (rec xs ws (1+ index))))))))
      (rec xs ws 0)
      xs)))

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
  (with-policy-expectations
      ((type (array color-component-type *) array)
       (type sequence dimensions)
       (type fixnum color-dimensions)
       (type sequence permutation)
       (type fixnum cutoff width height border-width)
       (type list border-color)
       (type boolean verbose)
       (assertion (%valid-permutation-p permutation (length dimensions))))
    (multiple-value-bind (widths heights) (%collect-width-and-height-components dimensions permutation cutoff)
      (let ((pixel-indexes (mapcar (constantly 0) (list* 1 (coerce dimensions 'list)))))
        (with-policy-expectations
            ((type list pixel-indexes))
          (flet ((map-indexes (is offset)
                   (loop :for i :across is
                         :for k :from offset
                         :do (setf (elt pixel-indexes (elt permutation k)) i)))
                 (output-border-pixel ()
                   (funcall output-pixel-fn border-color))
                 (output-pixel ()
                   (funcall output-pixel-fn (loop :for k :below color-dimensions
                                                  :collecting (progn
                                                                (setf (elt pixel-indexes (length dimensions)) k)
                                                                (apply #'aref array pixel-indexes))))))
            (loop :with ys := (map 'vector (constantly 0) heights)
                  :for y :below height
                  :for border-row-p := (%border-p ys heights)
                  :do (when verbose
                        (format *debug-io* "Scanline ~D of ~D~C" (1+ y) height #\Return))
                  :finally (when verbose
                             (format *debug-io* "Done====================================~%"))
                  :do (unless border-row-p
                        (map-indexes ys (length widths)))
                  :do (unwind-protect
                           (loop :with xs := (map 'vector (constantly 0) widths)
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
                        (%increment ys heights border-width)))))))))

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
  (with-policy-expectations
      ((type (array color-component-type *) array)
       (type sequence dimensions)
       (type fixnum color-dimensions output-color-dimensions)
       (type sequence permutation)
       (type fixnum cutoff)
       (type fixnum width height border-width)
       (type list border-color)
       (assertion (%valid-permutation-p permutation (length dimensions)))
       (returns (or null
                    pathname)))
    (let ((png (make-instance 'zpng:pixel-streamed-png
                              :color-type (ecase output-color-dimensions
                                            (1 :grayscale)
                                            (2 :grayscale-alpha)
                                            (3 :truecolor)
                                            (4 :truecolor-alpha))
                              :width width
                              :height height))
          (filename (merge-pathnames filename
                                     (make-pathname :type "png"))))
      (with-policy-expectations
          ((type zpng:pixel-streamed-png png)
           (type pathname filename))
        (labels ((clamp (n)
                   (let ((v (round (* #.(color-component 255) n))))
                     (cond
                       ((< v 0) #.(color-component 0))
                       ((< 255 v) #.(color-component 255))
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
          (with-open-file (stream filename
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :element-type '(unsigned-byte 8))
            (zpng:start-png png stream)
            (unwind-protect
                 (prog1
                     filename
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
                                   :output-pixel-fn #'output-pixel))
              (zpng:finish-png png))))))))

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
         (dimensions (coerce (butlast dimensions) 'vector))
         (permutation (or permutation
                          (loop :for i :below (length dimensions) :collecting i)))
         (cutoff (or cutoff
                     (ceiling (length permutation) 2)))
         (output-color-dimensions (length border-color))
         (border-color (mapcar #'color-component
                               (or (coerce border-color 'list)
                                   (loop :repeat color-dimensions :collecting 0)))))
    (check-type border-width (integer 0))
    (check-type border-color list)
    (assert (%valid-permutation-p permutation (length dimensions)) (permutation)
            "Permutation ~A does not word for dimensions ~D"
            permutation
            (length dimensions))
    (assert (or (zerop border-width)
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
