;;;; camera.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(defstruct (camera (:conc-name %camera-)
                   (:constructor %make-camera))
  (spatial-dimensions (error "Must specify SPATIAL-DIMENSIONS") :type spatial-dimensions-type :read-only t)
  (color-dimensions (error "Must specify COLOR-DIMENSIONS") :type color-dimensions-type :read-only t)
  (image-dimensions (error "Must specify IMAGE-DIMENSIONS") :type list :read-only t)
  (aspect-ratios (error "Must specify IMAGE-DIMENSIONS") :type list :read-only t)
  (viewport (error "Must specify VIEWPORT") :type list :read-only t)
  (center (error "Must specify CENTER") :type vec :read-only t)
  (max-depth (error "Must specify MAX-DEPTH") :type (integer 0 *) :read-only t))

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
                 (color-dimensions (error "Must specify COLOR-DIMENSIONS"))
                 (max-depth 50))
  (check-type width (integer 1 #.array-dimension-limit))
  (check-type aspect-ratios (or null real list))
  (check-type viewport (or null real list))
  (check-type center (or null vec))
  (check-type spatial-dimensions spatial-dimensions-type)
  (check-type color-dimensions color-dimensions-type)
  (check-type max-depth (integer 0 *))
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
                                  :aspect-ratios aspect-ratios
                                  :viewport viewport
                                  :center (apply #'vec (loop :repeat spatial-dimensions :collecting 0))
                                  :max-depth max-depth)))
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

(declaim (inline %make-black))
(defun %make-black (color-dimensions)
  (ecase color-dimensions
    (1
     #.(color 0))
    (2
     #.(color 0 1))
    (3
     #.(color 0 0 0))
    (4
     #.(color 0 0 0 1))))

(declaim (type (function (ray list spatial-dimensions-type (integer 0 *) color-dimensions-type) color) %ray-color-one))
(defun %ray-color-one (ray world spatial-dimensions color-dimensions max-depth sky-color)
  (with-policy-expectations
      ((type ray ray)
       (type list world)
       (type spatial-dimensions-type spatial-dimensions)
       (type color-dimensions-type color-dimensions)
       (type (integer 0 *) max-depth)
       (type color sky-color)
       (returns color))
    (cond
      ((plusp max-depth)
       (let ((hit (hit world ray (interval 1/10000 most-positive-fixnum))))
         (cond
           (hit
            (let* ((full (to-full-hit hit))
                   (p (point full))
                   (n (normal full))
                   (new-dir (v+ n (random-unit-vector spatial-dimensions))))
              (c* (%ray-color-one (ray p new-dir) world spatial-dimensions color-dimensions (1- max-depth) sky-color)
                  1/2)))
           (t
            sky-color))))
      (t
       (%make-black color-dimensions)))))

(declaim (inline %make-sample-ray)
         (type (function (ray list list) ray) %make-sample-ray))
(defun %make-sample-ray (ray pixel-deltas aspect-ratios)
  (declare (ignorable aspect-ratios))
  (with-policy-expectations
      ((type ray ray)
       (type list pixel-deltas aspect-ratios)
       (returns ray))
    (labels ((rnd ()
               (- (random #.(vector-component 1))
                  #.(vector-component 1/2)))
             (scale (delta)
               (let ((rr (rnd)))
                 (or #+(or)
                     delta
                     (apply #'vec
                            (vref delta 0)
                            (loop :for ii :from 1
                                  :for aa :in aspect-ratios
                                  :collecting (* (vref delta ii)
                                                 (/ rr
                                                    aa))))))))
      (with-ray (origin direction) ray
        (ray origin
             (loop :with sample-dir := direction
                   :for delta :in pixel-deltas
                   :for scaled-delta := (scale delta)
                   :do (setf sample-dir (v+ sample-dir scaled-delta))
                   :finally (return sample-dir)))))))

(declaim (inline %ray-color)
         (type (function (ray list spatial-dimensions-type color-dimensions-type (integer 0 *) color (integer 1 *) list list) color) %ray-color))
(defun %ray-color (ray world spatial-dimensions color-dimensions max-depth sky-color samples-per-pixel pixel-deltas aspect-ratios)
  (with-policy-expectations
      ((type ray ray)
       (type list world pixel-deltas aspect-ratios)
       (type spatial-dimensions-type spatial-dimensions samples-per-pixel)
       (type color-dimensions-type color-dimensions)
       (type (integer 0 *) max-depth)
       (type color sky-color)
       (returns color))
    (flet ((get-color (ray)
             (%ray-color-one ray world spatial-dimensions color-dimensions max-depth sky-color)))
      (loop :for sample :from 1 :to samples-per-pixel
            :for sample-ray := ray :then (%make-sample-ray ray pixel-deltas aspect-ratios)
            :for color := (get-color sample-ray) :then (clerp color
                                                              (get-color sample-ray)
                                                              (/ #.(color-component 1) sample))
            :finally (return color)))))

(defun %make-sky-color (color-dimensions)
  (ecase color-dimensions
    (1
     #.(color 1))
    (2
     #.(color 1 1))
    (3
     #.(color 1/2 1/2 1))
    (4
     #.(color 1/2 1/2 1 1))))

(defun %ensure-sky-color (sky-color color-dimensions)
  (etypecase sky-color
    (null
     (%make-sky-color color-dimensions))
    (color
     (assert (or (= (csize sky-color) color-dimensions)
                 (= (1+ (csize sky-color)) color-dimensions)))
     sky-color)))

(declaim (type (function (camera t &key (sky-color (or null color))) (array color-component-type *)) render))
(defun render (camera world &key sky-color (samples-per-pixel 1))
  (check-type camera camera)
  (check-type samples-per-pixel spatial-dimensions-type)
  (let* ((image-dimensions (%camera-image-dimensions camera))
         (spatial-dimensions (%camera-spatial-dimensions camera))
         (color-dimensions (%camera-color-dimensions camera))
         (max-depth (%camera-max-depth camera))
         (sky-color (%ensure-sky-color sky-color color-dimensions))
         (array-dimensions (append image-dimensions (list color-dimensions)))
         (img (make-array array-dimensions
                          :element-type 'color-component-type
                          :initial-element (color-component 0))))

    (let* ((focal-length 1.0d0)

           (viewport (%camera-viewport camera))

           (camera-center (%camera-center camera))
           (aspect-ratios (%camera-aspect-ratios camera))

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
                                            max-depth
                                            sky-color
                                            samples-per-pixel
                                            pixel-deltas
                                            aspect-ratios)
            :do (loop :with rmi := (apply #'array-row-major-index img ii)
                      :with cdims := (csize pixel-color)
                      :for jj :below color-dimensions
                      :do (setf (row-major-aref img (+ rmi jj))
                                (if (< jj cdims)
                                    (cref pixel-color jj)
                                    #.(color-component 1)))))

      img)))
