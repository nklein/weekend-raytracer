;;;; test/nullspace.lisp

(in-package #:weekend-raytracer/test)

(nst:def-test-group nullspace-tests ()
  (nst:def-test can-find-orthogonal-to-one-standard-vector (vec= #.(vec 0 1 0 0))
    (find-orthogonal (list (vec 1 0 0 0))))

  (nst:def-test can-find-orthogonal-to-two-vectors-without-elimination-needed (vec= #.(vec 0 0 1 0))
    (find-orthogonal (list (vec 1 0 0 0)
                           (vec 0 1 0 0))))

  (nst:def-test finds-null-if-no-elimination-possible (:eql nil)
    (find-orthogonal (list (vec 1 0)
                           (vec 0 1))))

  (nst:def-test can-find-orthogonal-with-elimination (vec= #.(vec -1/2 -1/2 1))
    (find-orthogonal (list (vec 1 1 1)
                           (vec 1/2 -1/2 0))))

  (nst:def-test can-make-full-span-with-one-to-go (:seq (vec= #.(vec 1 1 1))
                                                        (vec= #.(vec 2 -2 0))
                                                        (vec= #.(vec -1/2 -1/2 1)))
    (full-span (list (vec 1 1 1)
                     (vec 2 -2 0))))

  (nst:def-test can-make-full-span-with-two-to-go (:seq (vec= #.(vec 1 1 1))
                                                        (vec= #.(vec -1 1 0))
                                                        (vec= #.(vec -1/2 -1/2 1)))
    (full-span (list (vec 1 1 1))))
  )
