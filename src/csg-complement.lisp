;;;; csg-complement.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(defstruct (csg-complement (:conc-name %csg-complement-)
                             (:constructor %make-csg-complement (obj)))
  (obj (error "Must specify OBJ") :type t :read-only t))

(defmethod make-load-form ((object csg-complement) &optional environment)
  (declare (ignore environment))
  `(csg-complement ,(%csg-complement-obj object)))

(declaim (inline csg-complement)
         (type (function (t) csg-complement) csg-complement))
(defun csg-complement (obj)
  (with-policy-expectations
      ((type t obj)
       (returns csg-complement))
    (%make-csg-complement obj)))

(declaim (inline complement-partial-hit)
         (type (function (partial-hit) partial-hit)))
(defun complement-partial-hit (hit)
  (with-policy-expectations
      ((type partial-hit hit)
       (returns partial-hit))
    (let ((tt (tt hit))
          (front-face-p (not (front-face-p hit))))
      (flet ((thunk ()
               (let ((full (to-full-hit hit)))
                 (full-hit tt
                           front-face-p
                           (point full)
                           (normal full)
                           (%full-hit-material full)))))
        (partial-hit tt
                     front-face-p
                     #'thunk)))))

(defmethod hit ((obj csg-complement) ray tinterval)
  (with-policy-expectations
      ((type csg-complement obj)
       (type ray ray)
       (type interval tinterval)
       (returns (or null partial-hit)))
    (let ((hit (hit (%csg-complement-obj obj) ray tinterval)))
      (when hit
        (complement-partial-hit hit)))))

(defmethod hit* ((obj csg-complement) ray)
  (with-policy-expectations
      ((type csg-complement obj)
       (type ray ray)
       (returns list))
    (let ((hits (hit* (%csg-complement-obj obj) ray))
          (everywhere '#.(cons most-negative-vector-component-type
                               most-positive-vector-component-type)))
      (cond
        ((null hits)
         (list everywhere))
        ((equalp everywhere hits)
         nil)
        (t
         (labels ((rec (hits result)
                    (cond
                      ((null hits)
                       (nreverse result))
                      ((null result)
                       (destructuring-bind ((in . out) &rest hits) hits
                         (cond
                           ((realp in)
                            (rec hits (list (cons (complement-partial-hit out)
                                                  most-positive-vector-component-type))))
                           ((realp out)
                            (rec hits (list (cons most-negative-vector-component-type
                                                  (complement-partial-hit in)))))
                           (t
                            (rec hits (list (cons (complement-partial-hit out)
                                                  most-positive-vector-component-type)
                                            (cons most-negative-vector-component-type
                                                  (complement-partial-hit in))))))))
                      (t
                       (destructuring-bind ((in . out) &rest hits) hits
                         (setf (cdr (first result)) (complement-partial-hit in))
                         (if (realp out)
                             (rec hits result)
                             (rec hits (list* (cons (complement-partial-hit out)
                                                    most-positive-vector-component-type)
                                              result))))))))
           (rec hits nil)))))))
