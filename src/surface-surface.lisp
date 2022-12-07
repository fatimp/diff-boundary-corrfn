(in-package :non-trivial-surface-functions)

(declaim (type alex:positive-fixnum *lattice-elements* *ε-pixels*)
         (type (single-float 0.0) *ε-threshold* *ε-intersections*))

(defparameter *lattice-elements* 2000)
(defparameter *ε-pixels* 1)
(defparameter *ε-threshold* 0.03)
(defparameter *ε-intersections* 1f-3)

(sera:-> interface-candidates
         (diff:differentiable-multivariate single-float)
         (values list &optional))
(defun interface-candidates (function threshold)
  (declare (optimize (speed 3)))
  (let ((Δ (/ (float (1- *lattice-elements*))))
        candidates)
    (loop for i below *lattice-elements*
          for x = (1- (* 2 Δ i)) do
          (loop for j below *lattice-elements*
                for y = (1- (* 2 Δ j)) do
                (when (< (abs (- threshold
                                 (diff:dual-realpart
                                  (funcall function
                                           (list
                                            (diff:dual x 0.0)
                                            (diff:dual y 0.0))))))
                         *ε-threshold*)
                  (push (list x y) candidates))))
    candidates))

(sera:-> euclidean-metric
         (list list)
         (values single-float &optional))
(defun euclidean-metric (p1 p2)
  (declare (optimize (speed 3)))
  (let ((x1 (first p1))
        (y1 (second p1))
        (x2 (first p2))
        (y2 (second p2)))
    (declare (type single-float x1 x2 y1 y2))
    (sqrt (+ (expt (- x1 x2) 2)
             (expt (- y1 y2) 2)))))

(sera:-> intersection-candidates
         (diff:differentiable-multivariate single-float list)
         (values list &optional))
(defun intersection-candidates (function threshold shift)
  (declare (optimize (speed 3)))
  (let* ((interface-candidates
          (interface-candidates function threshold))
         (interface-candidates-tree
          (vp-trees:make-vp-tree interface-candidates
                                 #'euclidean-metric))
         intersection-candidates)
    (flet ((add-sf (x1 x2)
             (declare (type single-float x1 x2))
             (+ x1 x2)))
      (dolist (interface-candidate interface-candidates)
        (when (vp-trees:search-close interface-candidates-tree
                                     (mapcar #'add-sf interface-candidate shift)
                                     (/ (* 2.0 *ε-pixels*) *lattice-elements*)
                                     #'euclidean-metric)
          (push interface-candidate intersection-candidates))))
    intersection-candidates))

(sera:-> intersections
         (diff:differentiable-multivariate single-float list)
         (values list &optional))
(defun intersections (function threshold shift)
  (declare (optimize (speed 3)))
  (let ((candidates (intersection-candidates function threshold shift)))
    (remove-duplicates
     (mapcar
      (lambda (candidate)
        (cl-optim:adam
         (alex:rcurry #'sf/math:intersection-equation function threshold shift)
         candidate :η 1f-2))
     candidates)
     :test (lambda (p1 p2)
             (< (euclidean-metric p1 p2) *ε-intersections*)))))

(defun normalize (vector)
  (let ((norm (sqrt (reduce #'+ (mapcar (alex:rcurry #'expt 2) vector)))))
    (mapcar (alex:rcurry #'/ norm) vector)))

(defun dot (v1 v2)
  (reduce #'+ (mapcar #'* v1 v2)))

(sera:-> surface-surface
         (diff:differentiable-multivariate single-float list)
         (values single-float &optional))
(defun surface-surface (function threshold shift)
  (let ((intersections (intersections function threshold shift)))
    (reduce
     #'+
     (mapcar
      (lambda (intersection)
        (let ((gradient-here    (normalize (cl-forward-diff:ad-multivariate function intersection)))
              (gradient-shifted (normalize (cl-forward-diff:ad-multivariate
                                            function (mapcar #'+ intersection shift)))))
          (/ (sqrt (- 1.0 (expt (dot gradient-here gradient-shifted) 2))))))
      intersections)
     :initial-value 0.0)))
