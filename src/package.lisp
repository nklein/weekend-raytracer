;;;; package.lisp

(defpackage #:weekend-raytracer
  (:use #:cl)
  (:export #:set-optimization-level)
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
           #:v.)
  (:export #:ray
           #:rayp
           #:origin
           #:direction
           #:at)
  (:export #:*verbose*
           #:write-image))
