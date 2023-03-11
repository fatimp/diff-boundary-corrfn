(in-package :diff-boundary-corrfn/math)

(sera:-> sub-dual-double (diff:dual double-float)
         (values diff:dual &optional))
(declaim (inline sub-dual-double))
(defun sub-dual-double (x y)
  (- x y))

(sera:-> intersection-equation
         (list diff:differentiable-multivariate double-float list)
         (values diff:dual &optional))
(defun intersection-equation (coord function threshold shift)
  (declare (optimize (speed 3)))
  (+ (expt (- (funcall function coord) threshold) 2)
     (expt (- (funcall function (mapcar #'sub-dual-double coord shift)) threshold) 2)))

(sera:-> intersection-equation-3
         (list diff:differentiable-multivariate double-float list list)
         (values diff:dual &optional))
(defun intersection-equation-3 (coord function threshold shift1 shift2)
  (declare (optimize (speed 3)))
  (+ (expt (- (funcall function coord) threshold) 2)
     (expt (- (funcall function (mapcar #'sub-dual-double coord shift1)) threshold) 2)
     (expt (- (funcall function (mapcar #'sub-dual-double coord shift2)) threshold) 2)))
