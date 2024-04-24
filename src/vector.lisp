;;;; vector.lisp

(in-package #:weekend-raytracer)

(deftype vec () 'vector)

(declaim (inline vec))
(defun vec (&rest values)
  (map 'vec #'vector-component values))

(declaim (inline vref)
         (type (function (vec fixnum) vector-component-type) vref))
(defun vref (vec index)
  (svref vec index))

(declaim (inline v+)
         (type (function (vec vec) vec) v+))
(defun v+ (a b)
  (map 'vec #'+ a b))

(declaim (inline v-)
         (type (function (vec vec) vec) v-))
(defun v- (a b)
  (map 'vec #'- a b))

(declaim (inline v*)
         (type (function (vec vector-component-type) vec) v*))
(defun v* (vec scalar)
  (map 'vec (lambda (x) (* x scalar)) vec))

(declaim (inline v/)
         (type (function (vec vector-component-type) vec) v/))
(defun v/ (vec scalar)
  (map 'vec (lambda (x) (/ x scalar)) vec))

(declaim (inline vlen^2)
         (type (function (vec) vector-component-type) vlen^2))
(defun vlen^2 (vec)
  (reduce #'+ vec :key (lambda (x) (* x x))))

(declaim (inline vlen)
         (type (function (vec) vector-component-type) vlen))
(defun vlen (vec)
  (vector-component (sqrt (vlen^2 vec))))

(declaim (inline v.)
         (type (function (vec vec) vector-component-type) v.))
(defun v. (a b)
  (reduce #'+ (map 'list #'* a b)))
