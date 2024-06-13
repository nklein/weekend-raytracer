;;;; sphere.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(defstruct (sphere (:conc-name %sphere-)
                   (:constructor %make-sphere (center radius radius^2)))
  (center (error "Must specify CENTER") :type vec :read-only t)
  (radius (error "Must specify RADIUS") :type vector-component-type :read-only t)
  (radius^2 (error "Must specify RADIUS^2") :type vector-component-type :read-only t))

(defmethod make-load-form ((object sphere) &optional environment)
  (declare (ignore environment))
  `(sphere ,(%sphere-center object) ,(%sphere-radius object)))

(declaim (inline sphere)
         (type (function (list) sphere) sphere))
(defun sphere (center radius)
  (with-policy-expectations
      ((type vec center)
       (type real radius)
       (returns sphere))
    (let* ((r (vector-component radius))
           (r^2 (* r r)))
      (%make-sphere center r r^2))))

(declaim (inline spherep)
         (type (function (t) boolean) spherep))
(defun spherep (x)
  (with-policy-expectations
      ((returns boolean))
    (typep x 'sphere)))

(declaim (inline center)
         (type (function (sphere) vec) center))
(defun center (s)
  (with-policy-expectations
      ((type sphere s)
       (returns vec))
    (%sphere-center s)))

(declaim (inline radius)
         (type (function (sphere) vector-component-type) radius))
(defun radius (s)
  (with-policy-expectations
      ((type sphere s)
       (returns vector-component-type))
    (%sphere-radius s)))

(declaim (inline radius^2)
         (type (function (sphere) vector-component-type) radius^2))
(defun radius^2 (s)
  (with-policy-expectations
      ((type sphere s)
       (returns vector-component-type))
    (%sphere-radius^2 s)))

(defmacro with-sphere^2 ((center radius radius^2) sphere &body body)
  (let ((ss (gensym "SS-")))
    `(let ((,ss ,sphere))
       (let ((,center (center ,ss))
             (,radius (radius ,ss))
             (,radius^2 (radius^2 ,ss)))
         ,@body))))

(defun hit-sphere (sphere ray)
  (with-policy-expectations
      ((type sphere sphere)
       (type ray ray)
       (returns boolean))
    (with-sphere^2 (center radius radius^2) sphere
      (declare (ignore radius))
      (with-ray (origin direction) ray
        (let ((oc (v- center origin)))
          (let ((a (vlen^2 direction))
                (b (* #.(vector-component -2)
                      (v. direction oc)))
                (c (- (vlen^2 oc)
                      radius^2)))
            (let ((d (- (* b b)
                        (* #.(vector-component 4) a c)) ))
              (not (minusp d)))))))))
