(in-package :non-trivial-surface-functions/math)

(sera:-> gaussian-field
         ((simple-array sf/struct:gaussian))
         (values diff:differentiable-multivariate &optional))
(defun gaussian-field (gaussians)
  (declare (optimize (speed 3)))
  (lambda (coord)
    (destructuring-bind (x y) coord
      (declare (type diff:dual x y))
      (reduce
       (lambda (acc gaussian)
         (declare (type diff:dual acc))
         (multiple-value-bind (μ σ)
             (sera:deconstruct gaussian)
           (declare (type single-float σ))
           (destructuring-bind (center-x center-y) μ
             (declare (type single-float center-x center-y))
             (+ acc
                (/
                 (exp (/ (+ (expt (- x center-x) 2)
                            (expt (- y center-y) 2))
                         (* -2 (expt σ 2))))
                 σ)))))
       gaussians :initial-value #d(0.0 0.0)))))

(declaim (ftype diff:differentiable-multivariate max-metric))
(defun max-metric (coord)
  (declare (optimize (speed 3)))
  (destructuring-bind (x y) coord
    (declare (type diff:dual x y))
    (max (abs x)
         (abs y))))

(declaim (ftype diff:differentiable-multivariate l1-metric))
(defun l1-metric (coord)
  (declare (optimize (speed 3)))
  (destructuring-bind (x y) coord
    (declare (type diff:dual x y))
    (+ (abs (* 0.5 x))
       (abs y))))

(sera:-> intersection-equation
         (list diff:differentiable-multivariate single-float list)
         (values diff:dual &optional))
(defun intersection-equation (coord function threshold shift)
  (declare (optimize (speed 3))
           (type single-float threshold))
  (flet ((add (x1 x2)
           (declare (type diff:dual    x1)
                    (type single-float x2))
           (+ x1 x2)))
    (+ (expt (- (the diff:dual (funcall function coord)) threshold) 2)
       (expt (- (the diff:dual (funcall function (mapcar #'add coord shift))) threshold) 2))))
