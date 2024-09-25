;;;; test/fixtures.lisp

(in-package #:weekend-raytracer/test)

;;; For some reason, trying to make a fixture like:
;;;   (nst:def-fixtures materials ()
;;;      (blue-diffuse #.(lambertian (color 1/2 1/2 1)))
;;;      (blue-shiny #.(metal (color 1/2 1/2 1))))
;;; causes me to have blue-diffuse as undefined

(nst:def-fixtures materials-fixture (:special (blue-diffuse blue-shiny))
  (blue-diffuse #.(lambertian (color 1/2 1/2 1)))
  (blue-shiny #.(metal (color 1/2 1/2 1))))
