(in-package :diff-boundary-corrfn/math)

(sera:-> intersection-equation
         ((simple-array diff:dual (cl:*))
          diff:differentiable-multivariate
          double-float
          (simple-array double-float (cl:*)))
         (values diff:dual &optional))
(defun intersection-equation (coord function threshold shift)
  (declare (optimize (speed 3)))
  (+ (expt (- (funcall function coord) threshold) 2)
     (expt (- (funcall function (map '(vector diff:dual) #'- coord shift)) threshold) 2)))

(sera:-> intersection-equation-3
         ((simple-array diff:dual (cl:*))
          diff:differentiable-multivariate
          double-float
          (simple-array double-float (cl:*))
          (simple-array double-float (cl:*)))
         (values diff:dual &optional))
(defun intersection-equation-3 (coord function threshold shift1 shift2)
  (declare (optimize (speed 3)))
  (+ (expt (- (funcall function coord) threshold) 2)
     (expt (- (funcall function (map '(vector diff:dual) #'- coord shift1)) threshold) 2)
     (expt (- (funcall function (map '(vector diff:dual) #'- coord shift2)) threshold) 2)))
