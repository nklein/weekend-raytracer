;;;; material.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(defstruct material)

(defgeneric scatter (material ray hit)
  (:method (material ray (hit partial-hit))
    (call-next-method material ray (to-full-hit hit)))
  (:method ((material material) (ray ray) (hit full-hit))
    (values nil
            nil))
  (:documentation "The SCATTER function takes a MATERIAL, a RAY, and a HIT instance and returns:
   (OR  (VALUES COLOR RAY)
        (VALUES NIL NIL))
If a RAY is returned, it is the a ray scattered from the surface at the point of the hit."))

(defstruct (lambertian (:conc-name %lamb-)
                       (:constructor %make-lambertian (albedo))
                       (:include material))
  (albedo (error "Must specify ALBEDO") :type color :read-only t))

(defmethod make-load-form ((object lambertian) &optional environment)
  (declare (ignore environment))
  `(lambertian ,(%lamb-albedo object)))

(declaim (inline lambertian)
         (type (function (color) lambertian) lambertian))
(defun lambertian (albedo)
  (with-policy-expectations
      ((type color albedo)
       (returns lambertian))
    (%make-lambertian albedo)))

(defmethod scatter ((material lambertian) (ray ray) (hit full-hit))
  (let* ((p (point hit))
         (n (normal hit))
         (sd (v+ n (random-unit-vector (vsize n)))))
    (values (ray p
                 (if (near-zero sd)
                     n
                     sd))
            (%lamb-albedo material))))

(defstruct (metal (:conc-name %metal-)
                       (:constructor %make-metal (albedo))
                       (:include material))
  (albedo (error "Must specify ALBEDO") :type color :read-only t))

(defmethod make-load-form ((object metal) &optional environment)
  (declare (ignore environment))
  `(metal ,(%metal-albedo object)))

(declaim (inline metal)
         (type (function (color) metal) metal))
(defun metal (albedo)
  (with-policy-expectations
      ((type color albedo)
       (returns metal))
    (%make-metal albedo)))

(declaim (inline reflect)
         (type (function (vec vec) vec) reflect))
(defun reflect (v n)
  (with-policy-expectations
      ((type vec v n)
       (returns vec))
    (v- v
        (v* n
            (* #.(vector-component 2)
               (v. v n))))))

(defmethod scatter ((material metal) (ray ray) (hit full-hit))
  (let* ((p (point hit))
         (n (normal hit))
         (d (direction ray))
         (sd (reflect d n)))
    (values (ray p sd)
            (%metal-albedo material))))