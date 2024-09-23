;;;; test/box-muller.lisp

(in-package #:weekend-raytracer/test)

(nst:def-test-group box-muller-tests ()
  (nst:def-test test-with-known-inputs (:values (:equalp -83255) (:equalp -144203))
    (flet ((round-off (x)
             (round x 1/100000)))
      (reset-box-muller)
      (let* ((z0 (box-muller #.(vector-component 1/4) #.(vector-component 2/3)))
             (z1 (box-muller)))
        (values (round-off z0)
                (round-off z1)))))

  (nst:def-test does-not-return-nil (:true)
    (or (box-muller)
        (box-muller)
        (box-muller))))
