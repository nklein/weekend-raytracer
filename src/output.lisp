;;;; output.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(deftype index-vector () 'sequence)

(defun %default-permutation (spatial-dimensions)
  (loop :for ii :below (1- spatial-dimensions)
        :collecting ii))

(defun %valid-permutation-p (permutation spatial-dimensions)
  (let ((defaults (%default-permutation spatial-dimensions))
        (permutation (coerce permutation 'list)))
    (and (null (set-difference defaults permutation :test #'=))
         (null (set-difference permutation defaults :test #'=))
         (= (length permutation)
            (1- spatial-dimensions)))))

(defun %collect-width-and-height-components (dimensions permutation cutoff)
  (with-policy-expectations
      ((type (or null index-vector) dimensions permutation)
       (type fixnum cutoff)
       (assertion (%valid-permutation-p permutation (1+ (length dimensions)))))
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

(defun %make-border-p-array (ws border-width)
  (with-policy-expectations
      ((type index-vector ws)
       (type fixnum border-width)
       (returns (array boolean (*))))
    (labels ((list-of (nn vv)
               (loop :repeat nn :collecting vv))
             (rec (ws accum bw)
               (cond
                 (ws
                  (destructuring-bind (w &rest ws) ws
                    (rec ws
                         (loop :with border := (list-of bw t)
                               :for ii :below w
                               :when (plusp ii)
                                 :appending border
                               :appending accum)
                         (* bw 2))))
                 ((null ws)
                  accum))))
      (make-array (%calculate-one-output-size ws border-width)
                  :element-type 'boolean
                  :initial-contents (rec (rest ws)
                                         (list-of (first ws) nil)
                                         border-width)))))

(defun %border-p (xx border-p-array)
  (with-policy-expectations
      ((type fixnum xx)
       (type (array boolean (*)) border-p-array)
       (returns boolean))
    (aref border-p-array xx)))

(defun %calculate-one-output-size (ws border-width)
  (with-policy-expectations
      ((type index-vector ws)
       (type fixnum border-width)
       (returns fixnum))
    (labels ((rec (ws accum bw)
               (cond
                 ((null ws)
                  accum)
                 (t
                  (destructuring-bind (w &rest ws) ws
                    (rec ws
                         (- (* w (+ accum bw))
                            bw)
                         (* bw 2)))))))
      (rec (rest ws) (first ws) border-width))))

(defun %calculate-output-size (dimensions border-width permutation cutoff)
  (with-policy-expectations
      ((type (or null index-vector) dimensions permutation)
       (type fixnum border-width cutoff)
       (assertion (%valid-permutation-p permutation (1+ (length dimensions)))))
    (multiple-value-bind (widths heights) (%collect-width-and-height-components dimensions permutation cutoff)
      (values (with-policy-expectations
                  ((returns fixnum))
                (or (%calculate-one-output-size widths border-width)
                    1))
              (with-policy-expectations
                  ((returns fixnum))
                (or (%calculate-one-output-size heights border-width)
                    1))))))

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
       (assertion (%valid-permutation-p permutation (1+ (length dimensions)))))
    (multiple-value-bind (widths heights) (%collect-width-and-height-components dimensions permutation cutoff)
      (let ((pixel-indexes (mapcar (constantly 0) (list* 1 (coerce dimensions 'list))))
            (border-p-ys (%make-border-p-array heights border-width))
            (border-p-xs (%make-border-p-array widths border-width)))
        (with-policy-expectations
            ((type list pixel-indexes))
          (flet ((make-permuted-vector (orig into)
                   (loop :for i :below (length permutation)
                         :do (setf (elt into i) (elt orig (elt permutation i)))
                         :finally (return into)))
                 (make-unpermuted-vector (orig into)
                   (loop :for i :below (length permutation)
                         :do (setf (elt into (elt permutation i)) (elt orig i))
                         :finally (return into)))
                 (output-border-pixel ()
                   (funcall output-pixel-fn border-color))
                 (output-pixel ()
                   (funcall output-pixel-fn (loop :for k :below color-dimensions
                                                  :collecting (progn
                                                                (setf (elt pixel-indexes (length dimensions)) k)
                                                                (apply #'aref array pixel-indexes))))))
            (loop :with pp := (map 'list (constantly 0) dimensions)
                  :with maxes := (reverse (make-permuted-vector (map 'list #'identity dimensions)
                                                                (map 'list #'identity dimensions)))
                  :for y :below height
                  :for border-row-p := (%border-p y border-p-ys)
                  :do (when verbose
                        (format *debug-io* "Scanline ~D of ~D~C" (1+ y) height #\Return))
                  :finally (when verbose
                             (format *debug-io* "Done====================================~%"))
                  :do (unwind-protect
                           (loop :with xs := (map 'vector (constantly 0) widths)
                                 :for x :below width
                                 :for border-column-p := (or border-row-p
                                                             (%border-p x border-p-xs))
                                 :do (unwind-protect
                                          (if border-column-p
                                              (output-border-pixel)
                                              (progn
                                                (make-unpermuted-vector (reverse pp) pixel-indexes)
                                                (output-pixel)
                                                (increment-indexes pp maxes)))))))))))))

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
                           gamma
                           verbose)
  (with-policy-expectations
      ((type (array color-component-type *) array)
       (type sequence dimensions)
       (type fixnum color-dimensions output-color-dimensions)
       (type sequence permutation)
       (type fixnum cutoff)
       (type fixnum width height border-width)
       (type list border-color)
       (type real gamma)
       (assertion (%valid-permutation-p permutation (1+ (length dimensions))))
       (assertion (plusp gamma))
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
                                     (make-pathname :type "png")))
          (1/gamma (/ gamma)))
      (with-policy-expectations
          ((type zpng:pixel-streamed-png png)
           (type pathname filename))
        (labels ((clamp (n)
                   (let ((v (round (* #.(color-component 255) n))))
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
                 (gamma-correct-sample (s)
                   (expt s 1/gamma))
                 (gamma-correct-and-clamp-pixel (pixel)
                   (clamp-pixel (mapcar #'gamma-correct-sample pixel)))
                 (output-pixel (pixel)
                   (zpng:write-pixel (gamma-correct-and-clamp-pixel pixel) png)))
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
                      (verbose *verbose*)
                      (gamma 1.0d0))
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

The GAMMA is used to gamma correct the image samples.

If VERBOSE is non-NIL, the writing will output progress information.
The default value is taken from the *VERBOSE* special variable."
  (let* ((dimensions (array-dimensions array))
         (color-dimensions (the integer (first (last dimensions))))
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
    (assert (%valid-permutation-p permutation (1+ (length dimensions))) (permutation)
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
                        :gamma gamma
                        :verbose verbose))))
