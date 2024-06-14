;;;; camera.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(deftype spatial-dimensions-type () '(integer 1 *))
(deftype color-dimensions-type () '(integer 1 4))

(defstruct (camera (:conc-name %camera-)
                   (:constructor %make-camera))
  (spatial-dimensions (error "Must specify SPATIAL-DIMENSIONS") :type spatial-dimensions-type :read-only t)
  (color-dimensions (error "Must specify COLOR-DIMENSIONS") :type color-dimensions-type :read-only t)
  (image-dimensions (error "Must specify IMAGE-DIMENSIONS") :type list :read-only t)
  (viewport (error "Must specify VIEWPORT") :type list :read-only t)
  (center (error "Must specify CENTER") :type vec :read-only t))

(defun %default-aspect-ratios (spatial-dimensions)
  (list* 1
         (when (< 1 spatial-dimensions)
           (list* 16/9 (loop :repeat (- spatial-dimensions 3)
                             :collecting 64)))))

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

(defun %default-viewport (aspect-ratios)
  (loop :for ii :below (length aspect-ratios)
        :for angle := (case ii
                        ((0 1) (/ #.(vector-component 2)
                                  (elt aspect-ratios ii)))
                        (t #.(vector-component 3/2)))
        :collecting angle))

(defun %ensure-viewport-list (viewport aspect-ratios)
  (etypecase viewport
    (real
     (loop :for ii :below (length aspect-ratios)
           :collecting (/ viewport
                          (if (< ii 2)
                              (elt aspect-ratios ii)
                              1))))
    (null
     (%default-viewport aspect-ratios))
    (list
     viewport)))

(defun %ensure-center (center spatial-dimensions)
  (etypecase center
    (null (apply #'vec (loop :repeat spatial-dimensions
                             :collecting 0)))
    (vec
     center)))

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

(defun %make-image-dimensions (width aspect-ratios)
  (loop :for ii :below (length aspect-ratios)
        :collecting (max 1
                         (round (/ width
                                   (elt aspect-ratios ii))))))

(declaim (type (function (&key (width (integer 1 #.array-dimension-limit))
                               (aspect-ratios (or null real list))
                               (center (or null vec))
                               (viewport (or null real list))
                               (spatial-dimensions spatial-dimensions-type)
                               (color-dimensions color-dimensions-type)) camera) camera))
(defun camera (&key
                 (width (error "Must specify WIDTH"))
                 aspect-ratios
                 viewport
                 center
                 (spatial-dimensions (error "Must specify SPATIAL-DIMENSIONS"))
                 (color-dimensions (error "Must specify COLOR-DIMENSIONS")))
  (check-type width (integer 1 #.array-dimension-limit))
  (check-type aspect-ratios (or null real list))
  (check-type viewport (or null real list))
  (check-type center (or null vec))
  (check-type spatial-dimensions spatial-dimensions-type)
  (check-type color-dimensions color-dimensions-type)
  (let* ((aspect-ratios (%ensure-aspect-ratios-list aspect-ratios spatial-dimensions))
         (viewport (%ensure-viewport-list viewport aspect-ratios))
         (center (%ensure-center center spatial-dimensions)))
    (assert (= (length aspect-ratios)
               (1- spatial-dimensions)))
    (assert (= (length viewport)
               (1- spatial-dimensions)))
    (assert (= (vsize center)
               spatial-dimensions))

    (let ((image-dimensions (%make-image-dimensions width aspect-ratios)))
      (let ((camera (%make-camera :spatial-dimensions spatial-dimensions
                                  :color-dimensions color-dimensions
                                  :image-dimensions image-dimensions
                                  :viewport viewport
                                  :center (apply #'vec (loop :repeat spatial-dimensions :collecting 0)))))
        camera))))

(defun %make-viewport-deltas (viewport spatial-dimensions)
  (let ((zero-vec (loop :repeat spatial-dimensions :collecting 0)))
    (loop :for ii :below (1- spatial-dimensions)
          :collecting (let ((ret (copy-seq zero-vec)))
                        (setf (elt ret (1+ ii))
                              (if (evenp ii)
                                  (elt viewport ii)
                                  (- (elt viewport ii))))
                        (apply #'vec ret)))))

(defun %make-pixel-deltas (deltas image-dimensions)
  (loop :for ww :in image-dimensions
        :for dd :in deltas
        :collecting (v/ dd ww)))

(defun %make-focal-length-vec (focal-length spatial-dimensions)
  (apply #'vec focal-length
         (loop :repeat (1- spatial-dimensions)
               :collecting 0)))

(defun %increment-indexes (indexes maxes &optional (by 1))
  (labels ((inc (indexes maxes)
             (when indexes
               (cond
                 ;; if we can increment the tail of indexes, then
                 ;; we are done.
                 ((inc (rest indexes) (rest maxes))
                  indexes)
                 ;; otherwise, we need to increment the current
                 ((< (incf (first indexes) (if (rest indexes)
                                               1
                                               by))
                     (first maxes))
                  indexes)
                 ;; but if that hit the max, then reset it to zero
                 ;; and propagate the carry by returning NIL
                 (t
                  (setf (first indexes) 0)
                  nil)))))
    (inc indexes maxes)))

(defun %calculate-indexed-delta (indexes deltas)
  (reduce #'v+
          (loop :for index :in indexes
                :for delta :in deltas
                :collecting (v* delta index))))

(declaim (inline %ray-color)
         (type (function (ray list (integer 1 *) (integer 1 4)) color) %ray-color))
(let ((cntr 0))
  (defun %ray-color (ray world spatial-dimensions color-dimensions sky-color)
    (with-policy-expectations
        ((type ray ray)
         (type list world)
         (type (integer 1 *) spatial-dimensions)
         (type (integer 1 4) color-dimensions)
         (type color sky-color)
         (returns color))
      (let ((hit (hit world ray (interval 0 most-positive-fixnum))))
        (cond
          (hit
           (let ((n (normal (to-full-hit hit))))
             (when (< (incf cntr) 10)
               (format *debug-io* "N = ~A~%" n))
             (flet ((remap (ii)
                      (cond
                        ;; rotate the normal coordinates a bit for
                        ;; prettier colors
                        ((and (< ii 3)
                              (<= 3 color-dimensions))
                         (elt '(1 2 0) ii))
                        (t
                         ii)))
                    (to-color (ii)
                      (cond
                        ((< ii spatial-dimensions)
                         (/ (1+ (color-component (vref n ii)))
                            #.(color-component 2)))
                        (t
                         #.(color-component 1)))))
               (apply #'color (loop :for ii :below color-dimensions
                                    :for jj := (remap ii)
                                    :collecting (to-color jj))))))
          (t
           sky-color))))))

(defun %make-sky-color (color-dimensions)
  (ecase color-dimensions
    (1
     (color 1))
    (2
     (color 1 1))
    (3
     (color 1/2 1/2 1))
    (4
     (color 1/2 1/2 1 1))))

(defun %ensure-sky-color (sky-color color-dimensions)
  (etypecase sky-color
    (null
     (%make-sky-color color-dimensions))
    (color
     (assert (or (= (csize sky-color) color-dimensions)
                 (= (1+ (csize sky-color)) color-dimensions)))
     sky-color)))

(declaim (type (function (camera t &key (sky-color (or null color))) (array color-component-type *)) render))
(defun render (camera world &key sky-color)
  (check-type camera camera)
  (let* ((image-dimensions (%camera-image-dimensions camera))
         (spatial-dimensions (%camera-spatial-dimensions camera))
         (color-dimensions (%camera-color-dimensions camera))
         (sky-color (%ensure-sky-color sky-color color-dimensions))
         (array-dimensions (append image-dimensions (list color-dimensions)))
         (img (make-array array-dimensions
                          :element-type 'color-component-type
                          :initial-element (color-component 0))))

    (let* ((focal-length 1.0d0)

           (viewport (%camera-viewport camera))

           (camera-center (%camera-center camera))

           (deltas (%make-viewport-deltas viewport spatial-dimensions))
           (pixel-deltas (%make-pixel-deltas deltas image-dimensions))

           (viewport-upper-left (reduce #'v-
                                        (list* camera-center
                                               (%make-focal-length-vec focal-length spatial-dimensions)
                                               (mapcar (lambda (v)
                                                         (v/ v 2))
                                                       deltas))))
           (pixel-origin (v+ viewport-upper-left
                             (v/ (reduce #'v+ pixel-deltas)
                                 2)))
           (indexes (loop :repeat spatial-dimensions :collecting 0)))

      (loop :for ii := indexes :then (%increment-indexes indexes array-dimensions color-dimensions)
            :while ii
            :for loc := (v+ pixel-origin
                            (%calculate-indexed-delta ii pixel-deltas))
            :for ray-direction := (v- loc camera-center)
            :for ray := (ray camera-center ray-direction)
            :for pixel-color := (%ray-color ray
                                            world
                                            spatial-dimensions
                                            color-dimensions
                                            sky-color)
            :do (loop :with rmi := (apply #'array-row-major-index img ii)
                      :with cdims := (csize pixel-color)
                      :for jj :below color-dimensions
                      :do (setf (row-major-aref img (+ rmi jj))
                                (if (< jj cdims)
                                    (cref pixel-color jj)
                                    #.(color-component 1)))))

      img)))
