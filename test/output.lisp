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
  (nst:def-test empty-list-returns-empty-lists (:values (:eql nil) (:eql nil))
    (weekend-raytracer::%collect-width-and-height-components nil nil 1))

  (nst:def-test width-height (:values (:equalp '(10)) (:equalp '(20)))
    (weekend-raytracer::%collect-width-and-height-components '(10 20) '(0 1) 1))

  (nst:def-test all-width (:values (:equalp '(10 20)) (:eql nil))
    (weekend-raytracer::%collect-width-and-height-components '(10 20) '(0 1) 2))

  (nst:def-test width-height-swapped (:values (:equalp '(20)) (:equalp '(10)))
    (weekend-raytracer::%collect-width-and-height-components '(10 20) '(1 0) 1))

  (nst:def-test width-height-depth-ana (:values (:equalp '(10 20)) (:equalp '(5 2)))
    (weekend-raytracer::%collect-width-and-height-components '(10 20 5 2) '(0 1 2 3) 2)))

(nst:def-test-group output-%calculate-one-output-size-tests ()
  (nst:def-test one-frame (:eql 10)
    (weekend-raytracer::%calculate-one-output-size '(10) 1))

  (nst:def-test ten-by-three-frames (:eql 32)
    ;; +----------+ +----------+ +----------+
    ;; |0123456789|#|0123456789|#|0123456789|
    ;; +----------+ +----------+ +----------+
    (weekend-raytracer::%calculate-one-output-size '(10 3) 1))

  (nst:def-test ten-by-three-by-two-frames (:eql 66)
    ;; +----------+ +----------+ +----------+  +----------+ +----------+ +----------+
    ;; |0123456789|#|0123456789|#|0123456789|##|0123456789|#|0123456789|#|0123456789|
    ;; +----------+ +----------+ +----------+  +----------+ +----------+ +----------+
    (weekend-raytracer::%calculate-one-output-size '(10 3 2) 1))

  (nst:def-test five-by-three-by-three-frames (:eql 55)
    ;; +-----+ +-----+ +-----+  +-----+ +-----+ +-----+  +-----+ +-----+ +-----+
    ;; |01234|-|01234|-|01234|-#|01234|-|01234|-|01234|-#|01234|-|01234|-|01234|
    ;; +-----+ +-----+ +-----+  +-----+ +-----+ +-----+  +-----+ +-----+ +-----+
    (weekend-raytracer::%calculate-one-output-size '(5 3 3) 1)))

(nst:def-test-group output-%calculate-output-size-tests ()
  (nst:def-test empty-list-returns-single-pixel (:values (:eql 1) (:eql 1))
    (weekend-raytracer::%calculate-output-size nil 1 nil 1))

  (nst:def-test width-height-with-border (:values (:eql 10) (:eql 20))
    (weekend-raytracer::%calculate-output-size '(10 20) 1 '(0 1) 1))

  (nst:def-test width-height-swapped-with-border (:values (:eql 20) (:eql 10))
    (weekend-raytracer::%calculate-output-size '(10 20) 1 '(1 0) 1))

  (nst:def-test width-height-depth-no-border (:values (:eql 50) (:eql 20))
    (weekend-raytracer::%calculate-output-size '(10 5 20) 0 '(0 1 2) 2))

  (nst:def-test width-height-depth-border (:values (:eql 54) (:eql 20))
    (weekend-raytracer::%calculate-output-size '(10 5 20) 1 '(0 1 2) 2))

  (nst:def-test width-height-depth-big-border (:values (:eql 58) (:eql 20))
    (weekend-raytracer::%calculate-output-size '(10 5 20) 2 '(0 1 2) 2))

  (nst:def-test width-height-depth-ana-border (:values (:eql 54) (:eql 41))
    (weekend-raytracer::%calculate-output-size '(10 5 20 2) 1 '(0 1 2 3) 2)))

(nst:def-test-group output-%border-p-tests ()
  (nst:def-test not-border (:eql nil)
    (weekend-raytracer::%border-p '(3 4 5 6 7) '(12 11 10 9 8)))

  (nst:def-test border-at-position-zero (:true)
    (weekend-raytracer::%border-p '(3 4 5 6 7) '(3 11 10 9 8)))

  (nst:def-test border-at-last-position (:true)
    (weekend-raytracer::%border-p '(3 4 5 6 7) '(12 11 10 9 7)))

  (nst:def-test border-at-multiple-positions (:true)
    (weekend-raytracer::%border-p '(3 4 5 6 7) '(12 3 4 9 7))))

(nst:def-test-group output-%increment-tests ()
  (nst:def-test increment-zeros (:equalp '(1 0 0 0))
    (weekend-raytracer::%increment (list 0 0 0 0) '(2 2 2 2) 0))

  (nst:def-test increment-with-carry-no-border (:equalp '(0 1 0 0))
    (weekend-raytracer::%increment (list 1 0 0 0) '(2 2 2 2) 0))

  (nst:def-test increment-with-multiple-carries-no-border (:equalp '(0 0 0 1))
    (weekend-raytracer::%increment (list 1 2 3 0) '(2 3 4 5) 0))

  (nst:def-test increment-into-border (:equalp '(2 0 0 0))
    (weekend-raytracer::%increment (list 1 0 0 0) '(2 2 2 2) 1))

  (nst:def-test increment-within-border (:equalp '(3 0 0 0))
    (weekend-raytracer::%increment (list 2 0 0 0) '(2 2 2 2) 2))

  (nst:def-test increment-out-of-thin-border (:equalp '(0 1 0 0))
    (weekend-raytracer::%increment (list 2 0 0 0) '(2 2 2 2) 1))

  (nst:def-test increment-out-of-thick-border (:equalp '(0 1 0 0))
    (weekend-raytracer::%increment (list 3 0 0 0) '(2 2 2 2) 2)))
