;;;; vector.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(deftype vec () 'vector)

(declaim (inline vec)
         (type (function (&rest real) vec)))
(defun vec (&rest values)
  (with-policy-expectations
      ((assertion (every #'realp values))
       (returns vec))
    (map 'vec #'vector-component values)))

(declaim (inline vecp)
         (type (function (t) boolean) vecp))
(defun vecp (x)
  (with-policy-expectations
      ((returns boolean))
    (typep x 'vec)))

(declaim (inline vref)
         (type (function (vec fixnum) vector-component-type) vref))
(defun vref (vec index)
  (with-policy-expectations
      ((type vec vec)
       (type fixnum index)
       (returns vector-component-type))
    (svref vec index)))

(declaim (inline v+)
         (type (function (vec vec) vec) v+))
(defun v+ (a b)
  (with-policy-expectations
      ((type vec a)
       (type vec b)
       (assertion (= (length a) (length b)))
       (returns vec))
    (map 'vec #'+ a b)))

(declaim (inline v-)
         (type (function (vec vec) vec) v-))
(defun v- (a b)
  (with-policy-expectations
      ((type vec a)
       (type vec b)
       (assertion (= (length a) (length b)))
       (returns vec))
    (map 'vec #'- a b)))

(declaim (inline v*)
         (type (function (vec vector-component-type) vec) v*))
(defun v* (vec scalar)
  (with-policy-expectations
      ((type vec vec)
       (type vector-component-type scalar)
       (returns vec))
    (map 'vec (lambda (x) (* x scalar)) vec)))

(declaim (inline v/)
         (type (function (vec vector-component-type) vec) v/))
(defun v/ (vec scalar)
  (with-policy-expectations
      ((type vec vec)
       (type vector-component-type scalar)
       (returns vec))
    (map 'vec (lambda (x) (/ x scalar)) vec)))

(declaim (inline vlen^2)
         (type (function (vec) vector-component-type) vlen^2))
(defun vlen^2 (vec)
  (with-policy-expectations
      ((type vec vec)
       (returns vector-component-type))
    (reduce #'+ vec :key (lambda (x)
                           (* x x)))))

(declaim (inline vlen)
         (type (function (vec) vector-component-type) vlen))
(defun vlen (vec)
  (with-policy-expectations
      ((type vec vec)
       (returns vector-component-type))
    (vector-component (sqrt (vlen^2 vec)))))

(declaim (inline v.)
         (type (function (vec vec) vector-component-type) v.))
(defun v. (a b)
  (with-policy-expectations
      ((type vec a)
       (type vec b)
       (assertion (= (length a) (length b)))
       (returns vector-component-type))
    (reduce #'+ (map 'list #'* a b))))
