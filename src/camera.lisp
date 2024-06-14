;;;; camera.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(deftype spatial-dimensions-type () '(integer 1 *))
(deftype color-dimensions-type () '(integer 1 4))

(defstruct (camera (:conc-name %camera-)
                   (:constructor %make-camera))
  (spatial-dimensions (error "Must specify SPATIAL-DIMENSIONS") :type spatial-dimensions-type :read-only t)
  (color-dimensions (error "Must specify COLOR-DIMENSIONS") :type color-dimensions-type :read-only t)
  (aspect-ratios (error "Must specify ASPECT-RATIOS") :type list :read-only t)
  (image-dimensions (error "Must specify IMAGE-DIMENSIONS") :type list :read-only t))

(defun %default-aspect-ratios (spatial-dimensions)
  (list* 1
         (when (< 1 spatial-dimensions)
           (list* 16/9 (loop :repeat (- spatial-dimensions 3)
                             :collecting 5/320)))))

(defun %ensure-aspect-ratios-list (ratio spatial-dimensions)
  (etypecase ratio
    (real
     (list* 1
            (loop :repeat (- spatial-dimensions 2)
                  :collecting ratio)))
    (null
     (%default-aspect-ratios spatial-dimensions))
    (list
     ratio)))

(defun %best-border-color-dimensions-for (color-dimensions)
  (check-type color-dimensions color-dimensions-type)
  (ecase color-dimensions
    ((1 2)
     2)
    ((3 4)
     4)))

(defun %ensure-border-color (color color-dimensions)
  (if (colorp color)
      color
      (apply #'color (loop :repeat (%best-border-color-dimensions-for color-dimensions)
                           :collecting 0))))

(defun %ensure-permutation (permutation spatial-dimensions)
  (etypecase permutation
    (null
     (loop :for ii :below (1- spatial-dimensions)
           :collecting ii))
    (list
     permutation)))

(defun %valid-permutation-p (permutation spatial-dimensions)
  (let ((defaults (%ensure-permutation nil spatial-dimensions))
        (permutation (coerce permutation 'list)))
    (and (null (set-difference defaults permutation :test #'=))
         (null (set-difference permutation defaults :test #'=))
         (= (length permutation)
            (1- spatial-dimensions)))))

(defun %initialize-camera (camera)
  (declare (ignore camera)))

(declaim (type (function (&key (width (integer 1 #.array-dimension-limit))
                               (aspect-ratios (or null real list))
                               (border-width (integer 0 *))
                               (border-color (or null color))
                               (permutation list)
                               (spatial-dimensions spatial-dimensions-type)
                               (color-dimensions color-dimensions-type)) camera) camera))
(defun camera (&key
                 (width (error "Must specify WIDTH"))
                 aspect-ratios
                 (border-width 2)
                 border-color
                 permutation
                 (spatial-dimensions (error "Must specify SPATIAL-DIMENSIONS"))
                 (color-dimensions (error "Must specify COLOR-DIMENSIONS")))
  (check-type width (integer 1 #.array-dimension-limit))
  (check-type aspect-ratios (or null real list))
  (check-type border-width (integer 1 *))
  (check-type border-color (or null color))
  (check-type permutation (or null list))
  (check-type spatial-dimensions spatial-dimensions-type)
  (check-type color-dimensions color-dimensions-type)
  (let ((aspect-ratios (%ensure-aspect-ratios-list aspect-ratios spatial-dimensions))
        (border-color (when (plusp border-width)
                        (%ensure-border-color border-color color-dimensions)))
        (permutation (%ensure-permutation permutation spatial-dimensions)))
    (assert (= (length aspect-ratios)
               (1- spatial-dimensions)))
    (assert (or (not border-color)
                (= (csize border-color)
                   color-dimensions)
                (= (csize border-color)
                   (1+ color-dimensions))))
    (assert (%valid-permutation-p permutation spatial-dimensions))

    (let ((image-dimensions (mapcar (lambda (a)
                                      (max 1 (round (* width a))))
                                    aspect-ratios)))
      (let ((camera (%make-camera :spatial-dimensions spatial-dimensions
                                  :color-dimensions color-dimensions
                                  :image-dimensions image-dimensions
                                  :aspect-ratios aspect-ratios)))
        (prog1
            camera
          (%initialize-camera camera))))))

(declaim (inline colorp)
         (type (function (t) boolean) colorp))
(defun colorp (x)
  (with-policy-expectations
      ((returns boolean))
    (typep x 'color)))

(declaim (inline cref)
         (type (function (vec fixnum) color-component-type) cref))
(defun cref (color index)
  (with-policy-expectations
      ((type color color)
       (type fixnum index)
       (returns color-component-type))
    (aref (%color-vals color) index)))

(declaim (inline csize)
         (type (function (color) (integer 0 #.(1- array-dimension-limit))) csize))
(defun csize (color)
  (with-policy-expectations
      ((type color color)
       (returns (integer 0 #.(1- array-dimension-limit))))
    (array-dimension (%color-vals color) 0)))

(declaim (inline clerp)
         (type (function (color color real) color) clerp))
(defun clerp (a b tt)
  (with-policy-expectations
      ((type color a b)
       (type real tt)
       (assertion (= (csize a) (csize b)))
       (returns color))
    (flet ((lerp (a b)
             (color-component (+ (* a (- 1 tt))
                                 (* b tt)))))
      (let ((av (%color-vals a))
            (bv (%color-vals b)))
        (%color (loop :for ii :below (csize a)
                      :collecting (lerp (aref av ii) (aref bv ii))))))))
