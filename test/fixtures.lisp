;;;; test/fixtures.lisp

(in-package #:weekend-raytracer/test)

(nst:def-fixtures materials ()
  (blue-diffuse (lambertian (color 1/2 1/2 1)))
  (blue-shiny (metal (color 1/2 1/2 1))))
