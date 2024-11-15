;;;; csg-intersect.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(defstruct (csg-intersection (:conc-name %csg-intersection-)
                             (:constructor %make-csg-intersection (objs)))
  (objs (error "Must specify OBJS") :type list :read-only t))

(defmethod make-load-form ((object csg-intersection) &optional environment)
  (declare (ignore environment))
  `(csg-intersection ,@(%csg-intersection-objs object)))

(declaim (inline csg-intersection)
         (type (function (&rest t) csg-intersection) csg-intersection))
(defun csg-intersection (&rest objs)
  (with-policy-expectations
      ((type list objs)
       (returns csg-intersection))
    (%make-csg-intersection objs)))

(defmethod hit ((obj csg-intersection) ray tinterval)
  (with-policy-expectations
      ((type csg-intersection obj)
       (type ray ray)
       (type interval tinterval)
       (returns (or null partial-hit)))
    (loop :for (a . b) :in (hit* obj ray)
          :when (unless (realp a)
                  (surroundsp tinterval (tt a)))
            :do (return a)
          :when (unless (realp b)
                  (surroundsp tinterval (tt b)))
            :do (return b))))

(declaim (type (function (list list) list) csg-intersect-2))
(defun csg-intersect-2 (as bs)
  (labels ((rec (as bs result)
             (cond
               ((or (null as) (null bs))
                (nreverse result))
               (t
                (destructuring-bind ((a1 . a2) &rest new-as) as
                  (destructuring-bind ((b1 . b2) &rest new-bs) bs
                    (let ((a-t1 (tt a1))
                          (a-t2 (tt a2))
                          (b-t1 (tt b1))
                          (b-t2 (tt b2)))
                      (cond
                        ;; AAAA
                        ;;       BBBBB
                        ((< a-t2 b-t1)
                         (rec new-as bs result))
                        ;;       AAAA
                        ;; BBBBB
                        ((< b-t2 a-t1)
                         (rec as new-bs result))
                        ;; AAAAaaaaa
                        ;;   BBBBB
                        ((<= a-t1 b-t1 a-t2)
                         (if (<= b-t2 a-t2)
                             ;; AAAAAAAAA
                             ;;   BBBBB
                             (rec as new-bs (list* (cons b1 b2) result))
                             ;; AAAAA
                             ;;   BBBBB
                             (rec new-as bs (list* (cons b1 a2) result))))
                        ;;    AAAA
                        ;; BBBBBbbbbb
                        (t
                         (if (<= a-t2 b-t2)
                             ;;   AAAA
                             ;; BBBBBBBB
                             (rec new-as bs (list* (cons a1 a2) result))
                             ;;   AAAA
                             ;; BBBB
                             (rec as new-bs (list* (cons a1 b2) result))))))))))))
    (rec as bs nil)))

(defmethod hit* ((obj csg-intersection) ray)
  (with-policy-expectations
      ((type csg-intersection obj)
       (type ray ray)
       (returns list))
    (let ((objs (%csg-intersection-objs obj)))
      (cond
        ((null objs))
        ((null (second objs))
         (hit* (first objs) ray))
        (t
         (flet ((hit-1 (obj)
                  (hit* obj ray)))
           (loop :for obj :in objs
                 :for accum := (hit-1 obj) :then (csg-intersect-2 accum (hit-1 obj))
                 :while accum
                 :finally (return accum))))))))
