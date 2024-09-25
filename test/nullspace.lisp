;;;; test/nullspace.lisp

(in-package #:weekend-raytracer/test)

(defun find-and-add-orthogonal-vector (&rest vecs)
  (let ((ortho (find-orthogonal vecs)))
    (when ortho
      (list* ortho vecs))))

(nst:def-test-group nullspace-tests ()
  (nst:def-test can-find-orthogonal-to-one-standard-vector (orthogonal-set 2)
    (find-and-add-orthogonal-vector (vec 1 0 0 0)))

  (nst:def-test can-find-orthogonal-to-two-vectors-without-elimination-needed (orthogonal-set 3)
    (find-and-add-orthogonal-vector (vec 1 0 0 0)
                                    (vec 0 1 0 0)))

  (nst:def-test finds-null-if-no-elimination-possible (:eql nil)
    (find-orthogonal (list (vec 1 0)
                           (vec 0 1))))

  (nst:def-test can-find-orthogonal-with-elimination (orthogonal-set 3)
    (find-and-add-orthogonal-vector (vec 1 1 1)
                                    (vec 1/2 -1/2 0)))

  (nst:def-test can-make-full-span-with-one-to-go (orthogonal-set 3)
    (full-span (list (vec 1 1 1)
                     (vec 2 -2 0))))

  (nst:def-test can-make-full-span-with-two-to-go (orthogonal-set 3)
    (full-span (list (vec 1 1 1))))

  (nst:def-test can-orthogonalize-set (orthogonal-set 3)
    (orthogonalize (list (vec 1 1 1)
                         (vec 1 0 1)
                         (vec 1 2 2)))))
