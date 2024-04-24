;;;; types.lisp

(in-package #:weekend-raytracer)

(deftype vector-component-type () 'double-float)
(setf (documentation 'vector-component-type 'type) "Data type used for components of vectors"
      (documentation 'vector-component-type t)     (documentation 'vector-component-type 'type))

(declaim (inline vector-component))
(defun vector-component (x)
  "Convert X to the right type for a vector component"
  (coerce x 'vector-component-type))

(deftype color-component-type () 'double-float)
(setf (documentation 'color-component-type 'type) "Data type used for components of colors"
      (documentation 'color-component-type t)    (documentation 'color-component-type 'type))

(declaim (inline color-component))
(defun color-component (x)
  "Convert X to the right type for a color component"
  (coerce x 'color-component-type))
