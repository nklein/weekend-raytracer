;;;; types.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(deftype vector-component-type () 'double-float)
(setf (documentation 'vector-component-type 'type) "Data type used for components of vectors"
      (documentation 'vector-component-type t)     (documentation 'vector-component-type 'type))

(alexandria:define-constant most-negative-vector-component-type most-negative-double-float)
(alexandria:define-constant most-positive-vector-component-type most-positive-double-float)

(declaim (inline vector-component))
(defun vector-component (x)
  "Convert X to the right type for a vector component"
  (with-policy-expectations
      ((type real x)
       (returns vector-component-type))
    (coerce x 'vector-component-type)))

(deftype color-component-type () 'double-float)
(setf (documentation 'color-component-type 'type) "Data type used for components of colors"
      (documentation 'color-component-type t)    (documentation 'color-component-type 'type))

(declaim (inline color-component))
(defun color-component (x)
  "Convert X to the right type for a color component"
  (with-policy-expectations
      ((type real x)
       (returns color-component-type))
    (coerce x 'color-component-type)))

(deftype spatial-dimensions-type () '(integer 1 *))
(deftype spatial-dimension-index-type () '(integer 0 *))

(deftype color-dimensions-type () '(integer 1 4))
(deftype color-dimension-index-type () '(integer 0 3))
