;;;; package.lisp

(defpackage #:weekend-raytracer
  (:use #:cl)
  (:export #:set-optimization-level
           #:with-policy-expectations)
  (:export #:vector-component-type
           #:most-negative-vector-component-type
           #:most-positive-vector-component-type
           #:vector-component
           #:color-component-type
           #:color-component)
  (:export #:box-muller
           #:reset-box-muller)
  (:export #:interval
           #:imin
           #:imax
           #:with-interval
           #:surroundsp)
  (:export #:vec
           #:vecp
           #:vsize
           #:vref
           #:v+
           #:v-
           #:v*
           #:v/
           #:vlen^2
           #:vlen
           #:unit-vector
           #:random-unit-vector
           #:random-unit-vector-on-hemisphere
           #:v.
           #:mapv
           #:near-zero)
  (:export #:find-orthogonal
           #:full-span
           #:orthogonalize)
  (:export #:ray
           #:rayp
           #:origin
           #:direction
           #:at
           #:with-ray)
  (:export #:hit
           #:hit*
           #:tt
           #:front-face-p
           #:point
           #:normal
           #:full-hit
           #:partial-hit
           #:to-full-hit
           #:hit-material)
  (:export #:sphere
           #:center
           #:radius
           #:sphere-material
           #:with-sphere)
  (:export #:halfspace
           #:halfspace-normal
           #:halfspace-offset
           #:halfspace-material
           #:with-halfspace)
  (:export #:csg-intersection)
  (:export #:csg-complement)
  (:export #:csg-union)
  (:export #:color
           #:colorp
           #:cref
           #:clerp
           #:c*
           #:c*c)
  (:export #:material
           #:lambertian
           #:metal
           #:dialectric)
  (:export #:camera
           #:render
           #:increment-indexes
           #+bordeaux-threads #:*render-threads*)
  (:export #:*verbose*
           #:write-image))
