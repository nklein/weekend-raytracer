;;;; camera.lisp

(in-package #:weekend-raytracer)

#+bordeaux-threads
(defvar *render-threads* 1)

(set-optimization-level)

(declaim (inline degrees-to-radians)
         (type (function (real) vector-component-type) degrees-to-radians))
(defun degrees-to-radians (deg)
  (with-policy-expectations
      ((type real deg)
       (returns vector-component-type))
    (vector-component (* deg (/ pi 180)))))

(defstruct (camera (:conc-name %camera-)
                   (:constructor %make-camera))
  (spatial-dimensions (error "Must specify SPATIAL-DIMENSIONS") :type spatial-dimensions-type :read-only t)
  (color-dimensions (error "Must specify COLOR-DIMENSIONS") :type color-dimensions-type :read-only t)
  (image-dimensions (error "Must specify IMAGE-DIMENSIONS") :type list :read-only t)
  (aspect-ratios (error "Must specify IMAGE-DIMENSIONS") :type list :read-only t)
  (viewport (error "Must specify VIEWPORT") :type list :read-only t)
  (center (error "Must specify CENTER") :type vec :read-only t)
  (lookat nil :type (or null vec) :read-only t)
  (orientation nil :type list :read-only t)
  (max-depth (error "Must specify MAX-DEPTH") :type (integer 0 *) :read-only t)
  (field-of-view (error "Must specify FIELD-OF-VIEW") :type vector-component-type :read-only t)
  (focal-length (error "Must specify FOCAL-LENGTH") :type vector-component-type :read-only t)
  (focus-radius (error "Must specify FOCUS-RADIUS") :type vector-component-type :read-only t))

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
                               (lookat (or null vec))
                               (orientation list)
                               (spatial-dimensions spatial-dimensions-type)
                               (color-dimensions color-dimensions-type)
                               (max-depth (integer 1 *))
                               (field-of-view real)) camera) camera))
(defun camera (&key
                 (width (error "Must specify WIDTH"))
                 aspect-ratios
                 viewport
                 center
                 lookat
                 orientation
                 (spatial-dimensions (error "Must specify SPATIAL-DIMENSIONS"))
                 (color-dimensions (error "Must specify COLOR-DIMENSIONS"))
                 (max-depth 50)
                 (field-of-view 90)
                 (focal-length 1.0d0)
                 (focus-angle nil))
  (check-type width (integer 1 #.array-dimension-limit))
  (check-type aspect-ratios (or null real list))
  (check-type viewport (or null real list))
  (check-type center (or null vec))
  (check-type lookat (or null vec))
  (check-type orientation list)
  (check-type spatial-dimensions spatial-dimensions-type)
  (check-type color-dimensions color-dimensions-type)
  (check-type max-depth (integer 0 *))
  (check-type field-of-view real)
  (check-type focal-length real)
  (check-type focus-angle (or null real))
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
                                  :center center
                                  :lookat lookat
                                  :orientation orientation
                                  :max-depth max-depth
                                  :field-of-view (degrees-to-radians field-of-view)
                                  :focal-length (vector-component focal-length)
                                  :focus-radius (if (null focus-angle)
                                                    #.(vector-component 0)
                                                    (vector-component (* focal-length
                                                                         (tan (degrees-to-radians (/ focus-angle
                                                                                                     2)))))))))
        camera))))

(defun %make-viewport-deltas (viewport fov-factor camera-orientation)
  (loop :for oo :in (rest camera-orientation)
        :for vv :in viewport
        :for kk :from 0
        :collecting (let ((factor (* fov-factor vv)))
                      (v* oo (if (evenp kk) factor (- factor))))))

(defun %make-pixel-deltas (deltas image-dimensions)
  (loop :for ww :in image-dimensions
        :for dd :in deltas
        :collecting (v/ dd ww)))

(defun increment-indexes (indexes maxes &optional (by 1))
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
       (let ((hit (hit world ray (interval 1/5000 most-positive-fixnum))))
         (cond
           (hit
            (let* ((hit (to-full-hit hit))
                   (material (hit-material hit)))
              (multiple-value-bind (scattered attenuation) (scatter material ray hit)
                (cond
                  (scattered
                   (c*c (%ray-color-one scattered
                                        world
                                        spatial-dimensions
                                        color-dimensions
                                        (1- max-depth)
                                        sky-color)
                        attenuation))
                  (t
                   (%make-black color-dimensions))))))
           (t
            sky-color))))
      (t
       (%make-black color-dimensions)))))

(declaim (inline %make-sample-ray)
         (type (function (ray list list real list) ray) %make-sample-ray))
(defun %make-sample-ray (ray pixel-deltas aspect-ratios focus-radius camera-orientation)
  (with-policy-expectations
      ((type ray ray)
       (type list pixel-deltas aspect-ratios camera-orientation)
       (type real focus-radius)
       (returns ray))
    (labels ((rnd ()
               (- (random #.(vector-component 1))
                  #.(vector-component 1/2)))
             (scale (delta)
               (let ((rr (rnd)))
                 (apply #'vec
                        (vref delta 0)
                        (loop :for ii :from 1
                              :for aa :in aspect-ratios
                              :collecting (* (vref delta ii)
                                             (/ rr
                                                aa)))))))
      (with-ray (origin direction) ray
        (let ((new-origin (cond
                            ((plusp focus-radius)
                             (let ((offset (second (orthogonalize
                                                    (list (first camera-orientation)
                                                          (v* (random-unit-vector (vsize origin))
                                                              focus-radius))))))
                               (v+ origin offset)))
                            (t
                             origin))))
          (ray new-origin
               (loop :with sample-dir := direction
                     :for delta :in pixel-deltas
                     :for scaled-delta := (scale delta)
                     :do (setf sample-dir (v+ sample-dir scaled-delta))
                     :finally (return (if (eql origin new-origin)
                                          sample-dir
                                          (v+ sample-dir
                                              (v- origin new-origin)))))))))))

(declaim (inline %ray-color)
         (type (function (ray list spatial-dimensions-type color-dimensions-type (integer 0 *) color (integer 1 *) list list real list) color) %ray-color))
(defun %ray-color (ray world spatial-dimensions color-dimensions max-depth sky-color samples-per-pixel pixel-deltas aspect-ratios focus-radius camera-orientation)
  (with-policy-expectations
      ((type ray ray)
       (type list world pixel-deltas aspect-ratios)
       (type spatial-dimensions-type spatial-dimensions samples-per-pixel)
       (type color-dimensions-type color-dimensions)
       (type (integer 0 *) max-depth)
       (type color sky-color)
       (type real focus-radius)
       (type list camera-orientation)
       (returns color))
    (flet ((get-color (ray)
             (%ray-color-one ray world spatial-dimensions color-dimensions max-depth sky-color)))
      (loop :for sample :from 1 :to samples-per-pixel
            :for sample-ray := ray :then (%make-sample-ray ray pixel-deltas aspect-ratios
                                                           focus-radius camera-orientation)
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

(defun %ensure-camera-orientation (center lookat vecs)
  (mapcar #'unit-vector
          (full-span (orthogonalize (list* (v- lookat center) vecs)))))

(declaim (type (function (camera t &key (sky-color (or null color))) (array color-component-type *)) render))
(defun render (camera world &key sky-color (samples-per-pixel 1)
                              (threads (or #+bordeaux-threads *render-threads*
                                           1)))
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

    (when world
      (let* ((focal-length (%camera-focal-length camera))
             (focus-radius (%camera-focus-radius camera))

             (viewport (%camera-viewport camera))

             (field-of-view (%camera-field-of-view camera))
             (fov-factor (* focal-length (tan (/ field-of-view 2))))

             (camera-center (%camera-center camera))
             (camera-lookat (or (%camera-lookat camera)
                                (apply #'vec 1 (loop :repeat (1- spatial-dimensions) :collecting 0))))
             (aspect-ratios (%camera-aspect-ratios camera))

             (camera-orientation (%ensure-camera-orientation camera-center
                                                             camera-lookat
                                                             (%camera-orientation camera)))

             (deltas (%make-viewport-deltas viewport fov-factor camera-orientation))
             (pixel-deltas (%make-pixel-deltas deltas image-dimensions))

             (viewport-upper-left (reduce #'v-
                                          (list* (v+ camera-center (v* (first camera-orientation) focal-length))
                                                 (mapcar (lambda (v)
                                                           (v/ v 2))
                                                         deltas))))
             (pixel-origin (v+ viewport-upper-left
                               (v/ (reduce #'v+ pixel-deltas)
                                   2)))
             (indexes (loop :repeat spatial-dimensions :collecting 0))
             (done nil))

        (flet ((render-part (indexes maxes &optional verbose)
                 (loop :with index-offset := (first indexes)
                       :with last-start-index := index-offset
                       :for ii := indexes :then (increment-indexes indexes maxes color-dimensions)
                       :while ii
                       :until done
                       :for pixel :from 0
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
                                                       aspect-ratios
                                                       focus-radius
                                                       camera-orientation)
                       :when (and verbose
                                  (/= last-start-index (first ii)))
                         :do (prog1
                                 (setf last-start-index (first ii))
                               (format *debug-io* "~6,2,,0F% done~%" (/ (* 100.0d0 (- (first ii)
                                                                                      index-offset))
                                                                        (- (first maxes)
                                                                           index-offset))))
                       :do (loop :with rmi := (apply #'array-row-major-index img ii)
                                 :with cdims := (csize pixel-color)
                                 :for jj :below color-dimensions
                                 :do (setf (row-major-aref img (+ rmi jj))
                                           (if (< jj cdims)
                                               (cref pixel-color jj)
                                               #.(color-component 1)))))
                 indexes))
          (let* ((total-lines (first array-dimensions))
                 (lines-per-thread (ceiling total-lines threads))
                 (work (loop :for start :below total-lines :by lines-per-thread
                             :collecting (let ((ss (copy-seq indexes))
                                               (mm (copy-seq array-dimensions)))
                                           (setf (first ss) start
                                                 (first mm) (min total-lines
                                                                 (+ start lines-per-thread)))
                                           (format *debug-io* "Rendering ~A to ~A~%" ss mm)
                                           (lambda ()
                                             (let ((*box-muller-state* nil))
                                               (render-part ss mm (zerop (first ss)))))))))
            (unwind-protect
                 (or #+bordeaux-threads
                     (mapcar #'bt:join-thread (mapcar #'bt:make-thread work))
                     (mapcar #'funcall work))
              (setf done t))))))
    img))
