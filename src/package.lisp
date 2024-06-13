;;;; package.lisp

(defpackage #:weekend-raytracer
  (:use #:cl)
  (:export #:set-optimization-level
           #:with-policy-expectations)
  (:export #:vector-component-type
           #:vector-component
           #:color-component-type
           #:color-component)
  (:export #:vec
           #:vecp
           #:vref
           #:v+
           #:v-
           #:v*
           #:v/
           #:vlen^2
           #:vlen
           #:unit-vector
           #:v.
           #:mapv)
  (:export #:ray
           #:rayp
           #:origin
           #:direction
           #:at
           #:with-ray)
  (:export #:sphere
           #:center
           #:radius
           #:with-sphere)
  (:export #:color
           #:colorp
           #:cref
           #:clerp)
  (:export #:*verbose*
           #:write-image))
