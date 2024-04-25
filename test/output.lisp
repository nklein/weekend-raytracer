;;;; test/output.lisp

(in-package #:weekend-raytracer/test)

(nst:def-test-group output-%permutation-tests ()
  (nst:def-test in-order-is-valid (:true)
    (weekend-raytracer::%valid-permutation-p (list 0 1 2) 3))

  (nst:def-test random-order-is-valid (:true)
    (weekend-raytracer::%valid-permutation-p '(2 0 3 1) 4))

  (nst:def-test below-range-is-invalid (:not :true)
    (weekend-raytracer::%valid-permutation-p '(-1 0 2 1) 4))

  (nst:def-test above-range-is-invalid (:not :true)
    (weekend-raytracer::%valid-permutation-p '(1 0 2 4) 4))

  (nst:def-test duplicates-are-invalid (:not :true)
    (weekend-raytracer::%valid-permutation-p '(2 0 2 1) 4))

  (nst:def-test short-permutations-are-invalid (:not :true)
    (weekend-raytracer::%valid-permutation-p '(0 1 2) 4))

  (nst:def-test long-permutations-are-invalid (:not :true)
    (weekend-raytracer::%valid-permutation-p '(0 1 2 3 4 5) 4)))

(nst:def-test-group output-%collect-width-and-height-components-tests ()
  (nst:def-test empty-dimensions-returns-empty-vector (:values (:equalp #.(vector)) (:equalp #.(vector)))
    (weekend-raytracer::%collect-width-and-height-components nil nil 1))

  (nst:def-test width-height (:values (:equalp #.(vector 10)) (:equalp #.(vector 20)))
    (weekend-raytracer::%collect-width-and-height-components (vector 10 20) (vector 0 1) 1))

  (nst:def-test all-width (:values (:equalp #.(vector 10 20)) (:equalp #.(vector)))
    (weekend-raytracer::%collect-width-and-height-components (vector 10 20) (vector 0 1) 2))

  (nst:def-test width-height-swapped (:values (:equalp #.(vector 20)) (:equalp #.(vector 10)))
    (weekend-raytracer::%collect-width-and-height-components (vector 10 20) (vector 1 0) 1))

  (nst:def-test width-height-depth-ana (:values (:equalp #.(vector 10 20)) (:equalp #.(vector 5 2)))
    (weekend-raytracer::%collect-width-and-height-components (vector 10 20 5 2) (vector 0 1 2 3) 2)))

(nst:def-test-group output-%calculate-one-output-size-tests ()
  (nst:def-test one-frame (:eql 10)
    (weekend-raytracer::%calculate-one-output-size (vector 10) 1))

  (nst:def-test ten-by-three-frames (:eql 32)
    ;; +----------+ +----------+ +----------+
    ;; |0123456789|#|0123456789|#|0123456789|
    ;; +----------+ +----------+ +----------+
    (weekend-raytracer::%calculate-one-output-size (vector 10 3) 1))

  (nst:def-test ten-by-three-by-two-frames (:eql 66)
    ;; +----------+ +----------+ +----------+  +----------+ +----------+ +----------+
    ;; |0123456789|#|0123456789|#|0123456789|##|0123456789|#|0123456789|#|0123456789|
    ;; +----------+ +----------+ +----------+  +----------+ +----------+ +----------+
    (weekend-raytracer::%calculate-one-output-size (vector 10 3 2) 1))

  (nst:def-test five-by-three-by-three-frames (:eql 55)
    ;; +-----+ +-----+ +-----+  +-----+ +-----+ +-----+  +-----+ +-----+ +-----+
    ;; |01234|-|01234|-|01234|-#|01234|-|01234|-|01234|-#|01234|-|01234|-|01234|
    ;; +-----+ +-----+ +-----+  +-----+ +-----+ +-----+  +-----+ +-----+ +-----+
    (weekend-raytracer::%calculate-one-output-size (vector 5 3 3) 1)))

(nst:def-test-group output-%calculate-output-size-tests ()
  (nst:def-test empty-dimensions-returns-single-pixel (:values (:eql 1) (:eql 1))
    (weekend-raytracer::%calculate-output-size nil 1 nil 1))

  (nst:def-test width-height-with-border (:values (:eql 10) (:eql 20))
    (weekend-raytracer::%calculate-output-size (vector 10 20) 1 (vector 0 1) 1))

  (nst:def-test width-height-swapped-with-border (:values (:eql 20) (:eql 10))
    (weekend-raytracer::%calculate-output-size (vector 10 20) 1 (vector 1 0) 1))

  (nst:def-test width-height-depth-no-border (:values (:eql 50) (:eql 20))
    (weekend-raytracer::%calculate-output-size (vector 10 5 20) 0 (vector 0 1 2) 2))

  (nst:def-test width-height-depth-border (:values (:eql 54) (:eql 20))
    (weekend-raytracer::%calculate-output-size (vector 10 5 20) 1 (vector 0 1 2) 2))

  (nst:def-test width-height-depth-big-border (:values (:eql 58) (:eql 20))
    (weekend-raytracer::%calculate-output-size (vector 10 5 20) 2 (vector 0 1 2) 2))

  (nst:def-test width-height-depth-ana-border (:values (:eql 54) (:eql 41))
    (weekend-raytracer::%calculate-output-size (vector 10 5 20 2) 1 (vector 0 1 2 3) 2)))

(nst:def-test-group output-%border-p-tests ()
  (nst:def-test not-border (:eql nil)
    (weekend-raytracer::%border-p (vector 3 4 5 6 7) (vector 12 11 10 9 8)))

  (nst:def-test border-at-position-zero (:true)
    (weekend-raytracer::%border-p (vector 3 4 5 6 7) (vector 3 11 10 9 8)))

  (nst:def-test border-at-last-position (:true)
    (weekend-raytracer::%border-p (vector 3 4 5 6 7) (vector 12 11 10 9 7)))

  (nst:def-test border-at-multiple-positions (:true)
    (weekend-raytracer::%border-p (vector 3 4 5 6 7) (vector 12 3 4 9 7))))

(nst:def-test-group output-%increment-tests ()
  (nst:def-test increment-zeros (:equalp #.(vector 1 0 0 0))
    (weekend-raytracer::%increment (vector 0 0 0 0) (vector 2 2 2 2) 0))

  (nst:def-test increment-with-carry-no-border (:equalp #.(vector 0 1 0 0))
    (weekend-raytracer::%increment (vector 1 0 0 0) (vector 2 2 2 2) 0))

  (nst:def-test increment-with-multiple-carries-no-border (:equalp #.(vector 0 0 0 1))
    (weekend-raytracer::%increment (vector 1 2 3 0) (vector 2 3 4 5) 0))

  (nst:def-test increment-into-border (:equalp #.(vector 2 0 0 0))
    (weekend-raytracer::%increment (vector 1 0 0 0) (vector 2 2 2 2) 1))

  (nst:def-test increment-within-border (:equalp #.(vector 3 0 0 0))
    (weekend-raytracer::%increment (vector 2 0 0 0) (vector 2 2 2 2) 2))

  (nst:def-test increment-out-of-thin-border (:equalp #.(vector 0 1 0 0))
    (weekend-raytracer::%increment (vector 2 0 0 0) (vector 2 2 2 2) 1))

  (nst:def-test increment-out-of-thick-border (:equalp #.(vector 0 1 0 0))
    (weekend-raytracer::%increment (vector 3 0 0 0) (vector 2 2 2 2) 2)))
