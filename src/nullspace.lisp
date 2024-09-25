;;;; nullspace.lisp

(in-package #:weekend-raytracer)

(set-optimization-level)

(declaim (inline find-pivot-row-for-col)
         (type (function (list fixnum fixnum) (or null fixnum)) find-pivot-row-for-col))
(defun find-pivot-row-for-col (vecs col start-row)
  (loop :with best-row := start-row
        :with best-val := (abs (vref (elt vecs best-row) col))
        :for jj :from (1+ start-row)
        :for vec :in (nthcdr (1+ start-row) vecs)
        :do (let ((val (abs (vref vec col))))
              (when (< best-val val)
                (setf best-val val
                      best-row jj)))
        :finally (return (when (< 1/100000 best-val)
                           best-row))))

(declaim (inline eliminate-others)
         (type (function (list fixnum fixnum) list) eliminate-others))
(defun eliminate-others (vecs cur-row cur-col)
  (loop :with pivot-row := (elt vecs cur-row)
        :for ii :below (length vecs)
        :unless (= ii cur-row)
        :do (let ((cur (elt vecs ii)))
              (setf (elt vecs ii)
                    (v- cur
                        (v* pivot-row (vref cur cur-col))))))
  vecs)

(declaim (inline normalize-current)
         (type (function (list fixnum fixnum) list) normalize-current))
(defun normalize-current (vecs cur-row cur-col)
  (let ((pivot-val (vref (elt vecs cur-row) cur-col)))
    (setf (elt vecs cur-row) (v* (elt vecs cur-row) (/ pivot-val))))
  vecs)

(defun eliminate-all (vecs)
  (loop :with vsize := (vsize (first vecs))
        :with cur-col := 0
        :for cur-row :below (length vecs)
        :do (loop :while (< cur-col vsize)
                  :for pivot := (find-pivot-row-for-col vecs cur-col cur-row)
                  :do (cond
                        (pivot
                         (unless (= cur-row pivot)
                           (rotatef (elt vecs cur-row) (elt vecs pivot)))
                         (normalize-current vecs cur-row cur-col)
                         (eliminate-others vecs cur-row cur-col)
                         (return t))
                        (t
                         (incf cur-col)))))
  vecs)

(declaim (inline find-start-column)
         (type (function (vec) (or null fixnum)) find-start-column))
(defun find-start-column (vec)
  (loop :with found := nil
        :until found
        :for ii :below (vsize vec)
        :do (setf found
                  (< (abs (- (vref vec ii)
                             #.(vector-component 1)))
                     1/10000))
        :finally (return
                   (when found
                     ii))))

(defun find-orthogonal (vecs)
  (with-policy-expectations
      ((type list vecs)
       (assertion (not (null vecs)))
       (assertion (let ((size (vsize (first vecs))))
                    (every (lambda (vv)
                             (= (vsize vv) size))
                           (rest vecs))))
       (returns (or null vec)))
    (let* ((size (vsize (first vecs)))
           (vecs (eliminate-all (copy-seq vecs)))
           (col (when (< (length vecs) size)
                  (find-start-column (first (last vecs))))))
      (when col
        (apply #'vec
               (loop :with jj := 0
                     :with got-a-one := nil
                     :with col+1 := (1+ col)
                     :for ii :below size
                     :collecting (cond
                                   ((<= ii col)
                                    (let* ((vec (elt vecs jj))
                                           (coeff (vref vec ii)))
                                      (cond
                                        ((< #.(vector-component 1/2) coeff)
                                         (prog1
                                             (if (< col+1 size)
                                                 (- (vref vec col+1))
                                                 #.(vector-component 0))
                                           (incf jj)))
                                        (t
                                         (prog1
                                             (if got-a-one
                                                 #.(vector-component 0)
                                                 #.(vector-component 1))
                                           (setf got-a-one t))))))
                                   ((and (not got-a-one)
                                         (= ii (1+ col)))
                                    #.(vector-component 1))
                                   (t
                                    #.(vector-component 0)))))))))

(defun full-span (vecs)
  (with-policy-expectations
      ((type list vecs)
       (assertion (not (null vecs)))
       (assertion (let ((size (vsize (first vecs))))
                    (every (lambda (vv)
                             (= (vsize vv) size))
                           (rest vecs))))
       (returns list))
    (let ((vecs (reverse vecs)))
      (loop :with vsize := (vsize (first vecs))
            :for ii :from (length vecs) :below vsize
            :for vv := (find-orthogonal vecs)
            :unless vv
              :do (return-from full-span nil)
            :do (setf vecs (list* vv vecs)))
      (nreverse vecs))))

(defun orthogonalize (vecs)
  (flet ((proj (uu vv)
           (v* uu (/ (v. uu vv)
                     (vlen^2 uu)))))
    (let* ((u1 (first vecs))
           (uus (list u1)))
      (loop :for vv :in (rest vecs)
            :do (loop :with uu := (v- vv (proj u1 vv))
                      :for ww :in (rest uus)
                      :do (setf uu (v- uu (proj ww uu)))
                      :finally (nconc uus (list uu))))
      uus)))
