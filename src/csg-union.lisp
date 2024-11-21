;;;; csg-union.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(defstruct (csg-union (:conc-name %csg-union-)
                             (:constructor %make-csg-union (objs)))
  (objs (error "Must specify OBJS") :type list :read-only t))

(defmethod make-load-form ((object csg-union) &optional environment)
  (declare (ignore environment))
  `(csg-union ,@(%csg-union-objs object)))

(declaim (inline csg-union)
         (type (function (&rest t) csg-union) csg-union))
(defun csg-union (&rest objs)
  (with-policy-expectations
      ((type list objs)
       (returns csg-union))
    (%make-csg-union objs)))

(defmethod hit ((obj csg-union) ray tinterval)
  (with-policy-expectations
      ((type csg-union obj)
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

(declaim (type (function (list list) list) csg-union-2))
(defun csg-union-2 (as bs)
  (labels ((rec (as bs result)
             (cond
               ((and (null as) (null bs))
                (nreverse result))
               ((null as)
                (nconc (nreverse result) bs))
               ((null bs)
                (nconc (nreverse result) as))
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
                         (rec new-as bs (list* (cons a1 a2) result)))
                        ;;       AAAA
                        ;; BBBBB
                        ((< b-t2 a-t1)
                         (rec as new-bs (list* (cons b1 b2) result)))
                        ;; AAAAaaaaa
                        ;;   BBBBB
                        ((<= a-t1 b-t1 a-t2)
                         (if (<= b-t2 a-t2)
                             ;; AAAAAAAAA
                             ;;   BBBBB
                             (rec as new-bs result)
                             ;; AAAAA
                             ;;   BBBBB
                             (rec (list* (cons a1 b2) new-as) new-bs result)))
                        ;;    AAAA
                        ;; BBBBBbbbbb
                        (t
                         (if (<= a-t2 b-t2)
                             ;;   AAAA
                             ;; BBBBBBBB
                             (rec new-as bs result)
                             ;;   AAAA
                             ;; BBBB
                             (rec as (list* (cons b1 a2) new-bs) result)))))))))))
    (rec as bs nil)))

(defmethod hit* ((obj csg-union) ray)
  (with-policy-expectations
      ((type csg-union obj)
       (type ray ray)
       (returns list))
    (let ((objs (%csg-union-objs obj)))
      (cond
        ((null objs))
        ((null (second objs))
         (hit* (first objs) ray))
        (t
         (flet ((hit-1 (obj)
                  (hit* obj ray)))
           (loop :for obj :in objs
                 :for accum := (hit-1 obj) :then (csg-union-2 accum (hit-1 obj))
                 :finally (return accum))))))))
