;;;; package.lisp

(defpackage #:weekend-raytracer
  (:use #:cl)
  (:export #:vector-component-type
           #:vector-component
           #:color-component-type
           #:color-component)
  (:export #:vec
           #:vref
           #:v+
           #:v-
           #:v*
           #:v/
           #:vlen^2
           #:vlen
           #:v.)
  (:export #:*verbose*
           #:write-image))
