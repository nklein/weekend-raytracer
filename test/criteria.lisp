;;;; test/criteria.lisp

(in-package #:weekend-raytracer/test)


(nst:def-criterion (real= (given &optional (tolerance 1/100000)) (actual))
  (let ((delta (abs (- actual given))))
    (cond
      ((< delta tolerance)
       (nst:make-success-report))
      (t
       (nst:make-failure-report :format "Actual number ~A does not match given ~A"
                                :args (list actual given))))))

(nst:def-criterion (vec= (given &optional (tolerance 1/100000)) (actual))
  (let ((mag^2 (vlen^2 (v- actual given))))
    (cond
      ((< mag^2 tolerance)
       (nst:make-success-report))
      (t
       (nst:make-failure-report :format "Actual vector ~A does not match given ~A"
                                :args (list actual given))))))

(nst:def-criterion (orthogonal-set (vector-count &optional (tolerance 1/100000)) (actual))
  (let ((errs)
        (args))

    (unless (= vector-count (length actual))
      (push "~_Expected ~D vectors but got ~D" errs)
      (push (list vector-count (length actual)) args))

    (loop :for ii :from 0
          :for aa :in actual
          :do (cond
                (aa
                 (loop :for jj :from 0
                       :for bb :in actual
                       :do (cond
                             ((= ii jj)
                              (when (< (vlen aa) tolerance)
                                (push "~_v~D = ~A is degenerate" errs)
                                (push (list ii aa) args)))
                             ((or (null bb)
                                  (not (< ii jj)))
                              t)
                             ((= (vsize aa) (vsize bb))
                              (unless (< (abs (v. aa bb)) tolerance)
                                (push "~_v~D = ~A is not perpendicular to~_  v~D = ~A" errs)
                                (push (list ii aa jj bb) args)))
                             (t
                              (push "~_v~D = ~A different size than v~D = ~A" errs)
                              (push (list ii aa jj bb) args)))))
                (t
                 (push "~_v~D is nil" errs)
                 (push (list ii) args))))

    (if (null errs)
        (nst:make-success-report)
        (let ((fmt (apply #'concatenate 'string (nreverse errs)))
              (args (apply #'concatenate 'list (nreverse args))))
          (nst:make-failure-report :format fmt :args args)))))
