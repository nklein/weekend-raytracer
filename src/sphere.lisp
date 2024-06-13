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

(defmacro with-sphere ((center radius radius^2) sphere &body body)
  (let ((ss (gensym "SS-")))
    `(let ((,ss ,sphere))
       (let ((,center (center ,ss))
             (,radius (radius ,ss))
             (,radius^2 (radius^2 ,ss)))
         ,@body))))

(defmethod hit ((obj sphere) (ray ray) tmin tmax)
  (with-policy-expectations
      ((type sphere obj)
       (type ray ray)
       (type real tmin tmax)
       (returns (or null partial-hit)))
    (with-sphere (center radius radius^2) obj
      (with-ray (origin direction) ray
        (let ((oc (v- center origin)))
          (let ((a (vlen^2 direction))
                (h (v. direction oc))
                (c (- (vlen^2 oc)
                      radius^2)))
            (let ((d (- (* h h)
                        (* a c))))
              (unless (minusp d)
                (let ((sqrtd (the vector-component-type (sqrt d)))
                      (tmin (vector-component tmin))
                      (tmax (vector-component tmax)))
                  (flet ((for-root (tt)
                           (when (<= tmin tt tmax)
                             (flet ((thunk ()
                                      (let* ((point (at ray tt))
                                             (normal (v/ (v- point center)
                                                         radius))
                                             (front-face-p (minusp (v. direction
                                                                       normal))))
                                        (full-hit tt
                                                  front-face-p
                                                  point
                                                  (if front-face-p
                                                      normal
                                                      (v* normal
                                                          #.(vector-component -1)))))))
                               (partial-hit tt #'thunk)))))
                    (or (for-root (/ (- h sqrtd) a))
                        (for-root (/ (+ h sqrtd) a)))))))))))))
