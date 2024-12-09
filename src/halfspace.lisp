;;;; halfspace.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(defstruct (halfspace (:conc-name %halfspace-)
                      (:constructor %make-halfspace (normal offset material)))
  (normal (error "Must specify NORMAL") :type vec :read-only t)
  (offset (error "Must specify OFFSET") :type vector-component-type :read-only t)
  (material (error "Must specify MATERIAL") :type material :read-only t))

(defmethod make-load-form ((object halfspace) &optional environment)
  (declare (ignore environment))
  `(halfspace ,(%halfspace-normal object) ,(%halfspace-offset object) ,(%halfspace-material object)))

(declaim (inline halfspace)
         (type (function (vec (or real vec) material) halfspace) halfspace))
(defun halfspace (normal offset material)
  (with-policy-expectations
      ((type vec normal)
       (type (or real vec) offset)
       (type material material)
       (returns halfspace))
    (let ((len (vlen normal))
          (normal (unit-vector normal)))
      (%make-halfspace normal
                       (if (realp offset)
                           (vector-component (/ offset len))
                           (v. normal offset))
                       material))))

(declaim (inline halfspacep)
         (type (function (t) boolean) halfspacep))
(defun halfspacep (x)
  (with-policy-expectations
      ((returns boolean))
    (typep x 'halfspace)))

(declaim (inline halfspace-normal)
         (type (function (halfspace) vec) halfspace-normal))
(defun halfspace-normal (s)
  (with-policy-expectations
      ((type halfspace s)
       (returns vec))
    (%halfspace-normal s)))

(declaim (inline halfspace-offset)
         (type (function (halfspace) vector-component-type) halfspace-offset))
(defun halfspace-offset (s)
  (with-policy-expectations
      ((type halfspace s)
       (returns vector-component-type))
    (%halfspace-offset s)))

(declaim (inline halfspace-material)
         (type (function (halfspace) material) halfspace-material))
(defun halfspace-material (s)
  (with-policy-expectations
      ((type halfspace s)
       (returns material))
    (%halfspace-material s)))

(defmacro with-halfspace ((normal offset material) halfspace &body body)
  (let ((ss (gensym "SS-")))
    `(let ((,ss ,halfspace))
       (let ((,normal (halfspace-normal ,ss))
             (,offset (halfspace-offset ,ss))
             (,material (halfspace-material ,ss)))
         ,@body))))

(defmethod hit ((obj halfspace) ray tinterval)
  (with-policy-expectations
      ((type halfspace obj)
       (type ray ray)
       (type interval tinterval)
       (returns (or null partial-hit)))
    (with-halfspace (normal offset material) obj
      (with-ray (origin direction) ray
        (let* ((direction.normal (v. direction normal))
               (front-face-p (minusp direction.normal)))
          (when (< 1E-8 (abs direction.normal))
            (let ((tt (/ (- offset (v. origin normal))
                         direction.normal)))
              (when (surroundsp tinterval tt)
                (flet ((thunk ()
                         (let* ((point (at ray tt)))
                           (full-hit tt
                                     front-face-p
                                     point
                                     (if front-face-p
                                         normal
                                         (v* normal #.(vector-component -1)))
                                     material))))
                  (partial-hit tt front-face-p #'thunk))))))))))

(defmethod hit* ((obj halfspace) ray)
  (with-policy-expectations
      ((type halfspace obj)
       (type ray ray)
       (returns list))
    (with-halfspace (normal offset material) obj
      (with-ray (origin direction) ray
        (let* ((direction.normal (v. direction normal))
               (front-face-p (minusp direction.normal)))
          (flet ((make-partial (tt front-face-p normal)
                   (flet ((thunk ()
                            (let ((point (at ray tt)))
                              (full-hit tt
                                        front-face-p
                                        point
                                        (if front-face-p
                                            normal
                                            (v* normal #.(vector-component -1)))
                                        material))))
                     (partial-hit tt front-face-p #'thunk))))
            (cond
              ((< 1E-8 (abs direction.normal))
               (let ((tt (/ (- offset (v. origin normal))
                            direction.normal)))
                 (cond
                   (front-face-p
                    (list (cons (make-partial tt front-face-p normal)
                                (make-partial most-positive-vector-component-type
                                              front-face-p
                                              (v* (unit-vector direction)
                                                  #.(vector-component -1))))))
                   (t
                    (list (cons (make-partial most-negative-vector-component-type
                                              front-face-p
                                              (unit-vector direction))
                                (make-partial tt front-face-p normal)))))))
              (t
               (unless (minusp (- offset (v. origin normal)))
                 (list (cons (make-partial most-negative-vector-component-type
                                           front-face-p
                                           (unit-vector direction))
                             (make-partial most-positive-vector-component-type
                                           front-face-p
                                           (v* (unit-vector direction)
                                               #.(vector-component -1))))))))))))))
