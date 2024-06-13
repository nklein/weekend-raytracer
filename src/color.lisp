;;;; color.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(defstruct (color (:conc-name %color-)
                (:constructor %make-color (vals)))
  (vals (error "Must specify VALS") :type (simple-array color-component-type 1) :read-only t))

(defmethod make-load-form ((object color) &optional environment)
  (declare (ignore environment))
  `(color ,@(map 'list #'identity (%color-vals object))))

(declaim (inline %color)
         (type (function (list) color) %color))
(defun %color (vals)
  (with-policy-expectations
      ((assertion (every #'realp vals))
       (returns color))
    (%make-color (make-array (list (length vals))
                             :element-type 'color-component-type
                             :initial-contents (mapcar #'color-component vals)))))

(declaim (inline color)
         (type (function (&rest real) color) color))
(defun color (&rest vals)
  (with-policy-expectations
      ((type list vals)
       (returns color))
    (%color vals)))

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
    (aref (%color-vals color) index)))

(declaim (inline csize)
         (type (function (color) (integer 0 #.(1- array-dimension-limit))) csize))
(defun csize (color)
  (with-policy-expectations
      ((type color color)
       (returns (integer 0 #.(1- array-dimension-limit))))
    (array-dimension (%color-vals color) 0)))

(declaim (inline clerp)
         (type (function (color color real) color) clerp))
(defun clerp (a b tt)
  (with-policy-expectations
      ((type color a b)
       (type real tt)
       (assertion (= (csize a) (csize b)))
       (returns color))
    (flet ((lerp (a b)
             (color-component (+ (* a (- 1 tt))
                                 (* b tt)))))
      (let ((av (%color-vals a))
            (bv (%color-vals b)))
        (%color (loop :for ii :below (csize a)
                      :collecting (lerp (aref av ii) (aref bv ii))))))))
