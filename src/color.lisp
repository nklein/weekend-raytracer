;;;; color.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(deftype color () 'vector)

(declaim (inline color)
         (type (function (&rest real) color)))
(defun color (&rest values)
  (with-policy-expectations
      ((assertion (every #'realp values))
       (returns color))
    (map 'color #'color-component values)))

(declaim (inline colorp)
         (type (function (t) boolean) colorp))
(defun colorp (x)
  (with-policy-expectations
      ((returns boolean))
    (typep x 'color)))

(declaim (inline cref)
         (type (function (vec fixnum) color-component-type) cref))
(defun cref (color index)
  (with-policy-expectations
      ((type color color)
       (type fixnum index)
       (returns color-component-type))
    (svref color index)))

(declaim (inline clerp)
         (type (function (color color real) color) clerp))
(defun clerp (a b tt)
  (with-policy-expectations
      ((type color a b)
       (type real tt)
       (assertion (= (length a) (length b)))
       (returns color))
    (flet ((lerp (a b)
             (color-component (+ (* a (- 1 tt))
                                 (* b tt)))))
      (map 'color #'lerp a b))))
