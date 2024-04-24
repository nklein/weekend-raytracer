;;;; test/run.lisp

(in-package #:weekend-raytracer/test)

(defun run-all-tests (&key
                        (print-pretty *print-pretty*)
                        (debug-on-error nst:*debug-on-error*)
                        (debug-on-fail nst:*debug-on-fail*))
  (let ((*print-pretty* print-pretty)
        (nst:*debug-on-error* debug-on-error)
        (nst:*debug-on-fail* debug-on-fail))
    (nst:nst-cmd :run-package #.*package*)))
