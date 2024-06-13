;;;; hit.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(defstruct (full-hit (:conc-name %full-hit-)
                     (:constructor %make-full-hit (tt front-face-p point normal)))
  (tt (error "Must provide TT") :type vector-component-type :read-only t)
  (front-face-p (error "Must provide FRONT-FACE") :type boolean :read-only t)
  (point (error "Must provide POINT") :type vec :read-only t)
  (normal (error "Must provide NORMAL") :type vec :read-only t))

(declaim (inline full-hit)
         (type (function (real boolean vec vec) full-hit) full-hit))
(defun full-hit (tt front-face-p point normal)
  (with-policy-expectations
      ((type real tt)
       (type boolean front-face-p)
       (type vec point normal)
       (assertion (let ((scale 100000))
                    (= (round (vlen^2 normal) (/ scale))
                       scale)))
       (returns full-hit))
    (%make-full-hit (vector-component tt) front-face-p point normal)))

(defstruct (partial-hit (:conc-name %partial-hit-)
                        (:constructor %make-partial-hit (tt thunk)))
  (tt (error "Must provide TT") :type vector-component-type :read-only t)
  (thunk (error "Must provide THUNK") :type (function () full-hit) :read-only t)
  (full-hit nil :type (or null full-hit)))

(declaim (inline partial-hit)
         (type (function (real (function () full-hit)) partial-hit) partial-hit))
(defun partial-hit (tt thunk)
  (with-policy-expectations
      ((type real tt)
       (type function thunk)
       (returns partial-hit))
    (%make-partial-hit (vector-component tt) thunk)))

(declaim (inline to-full-hit)
         (type (function (partial-hit) full-hit) to-full-hit))
(defun to-full-hit (hit)
  (with-policy-expectations
      ((type partial-hit hit)
       (returns full-hit))
    (or (%partial-hit-full-hit hit)
        (setf (%partial-hit-full-hit hit)
              (funcall (%partial-hit-thunk hit))))))

(declaim (inline front-face-p)
         (type (function (full-hit) boolean) front-face-p))
(defun front-face-p (hit)
  (with-policy-expectations
      ((type full-hit hit)
       (returns boolean))
    (%full-hit-front-face-p hit)))

(declaim (inline point)
         (type (function (full-hit) vec) point))
(defun point (hit)
  (with-policy-expectations
      ((type full-hit hit)
       (returns vec))
    (%full-hit-point hit)))

(declaim (inline normal)
         (type (function (full-hit) vec) normal))
(defun normal (hit)
  (with-policy-expectations
      ((type full-hit hit)
       (returns vec))
    (%full-hit-normal hit)))

(defgeneric tt (hit)
  (:method ((hit full-hit))
    (with-policy-expectations
        ((type full-hit hit)
         (returns vector-component-type))
      (%full-hit-tt hit)))

  (:method ((hit partial-hit))
    (with-policy-expectations
        ((type partial-hit hit)
         (returns vector-component-type))
      (%partial-hit-tt hit))))

(defgeneric hit (obj ray tinterval)
  (:method ((obj list) ray tinterval)
    (with-policy-expectations
        ((type list obj)
         (type ray ray)
         (type interval tinterval)
         (returns (or null partial-hit)))
      (let ((tt (imax tinterval))
            (ret nil))
        (dolist (oo obj ret)
          (let ((hit (hit oo ray (interval (imin tinterval) tt))))
            (when hit
              (setf tt (tt hit)
                    ret hit))))))))
