;;;; test/color.lisp

(in-package #:weekend-raytracer/test)

(nst:def-test-group color-construction-tests ()
  (nst:def-test can-construct-an-rgb-color (:true)
    (color 1/3 1/2 2/3))

  (nst:def-test can-load-a-color (:true)
    #.(color 1/3 1/2 2/3))

  (nst:def-test can-construct-a-grayscale-color (:true)
    (color 1/2))

  (nst:def-test constructor-creates-a-color (:true)
    (colorp (color 1/3 1/2 2/3)))

  (nst:def-test constructor-upgrades-types (:true)
    (typep (cref (color 1/2) 0) 'color-component-type)))

(nst:def-test-group clerp-tests ()
  (nst:def-test clerp-starts-at-color1 (:eql 25)
    (values (round (cref (clerp (color 1/4) (color 3/4) 0) 0) 1/100)))

  (nst:def-test clerp-ends-at-color2 (:eql 75)
    (values (round (cref (clerp (color 1/4) (color 3/4) 1) 0) 1/100)))

  (nst:def-test clerp-interpolates (:eql 50)
    (values (round (cref (clerp (color 1/4) (color 3/4) 1/2) 0) 1/100))))
