(in-package :diff-boundary-corrfn/math)

(sera:defconstructor gaussian
  (μ list)
  (σ double-float))

(sera:-> random-gaussians
         (alex:positive-fixnum alex:positive-fixnum)
         (values (simple-array gaussian (cl:*)) &optional))
(defun random-gaussians (ngauss ndims)
  "Generate NGAUSS NDIMS-dimensional bell-shaped functions with random
peak and sharpness."
  (make-array ngauss
              :element-type 'gaussian
              :initial-contents
              (loop repeat ngauss collect
                    (gaussian (loop repeat ndims
                                    for μ = (cl:- (random 1.5d0) 0.75d0)
                                    collect μ)
                              (cl:+ 0.1d0 (random 0.3d0))))))

(sera:-> gaussian-field
         ((simple-array gaussian (cl:*)))
         (values diff:differentiable-multivariate &optional))
(defun gaussian-field (gaussians)
  "Return a function which calculates a sum of gaussians defined by
GAUSSIANS at the point COORD. GAUSSIANS may be generated randomly by
calling RANDOM-GAUSSIANS."
  (declare (optimize (speed 3)))
  (lambda (coord)
    (reduce
     (lambda (acc gaussian)
       (declare (type diff:dual acc))
       (let ((μ (gaussian-μ gaussian))
             (σ (gaussian-σ gaussian)))
         (+ acc
            (/ (exp (/
                     (the diff:dual
                          (reduce #'+
                                  (map '(vector diff:dual)
                                       (lambda (x center-x)
                                         (declare (type double-float center-x)
                                                  (type diff:dual x))
                                         (expt (- x center-x) 2))
                                       coord μ)))
                     (* -2 (expt σ 2))))
               σ))))
     gaussians :initial-value #d(0d0 0d0))))

(sera:-> intersection-equation
         (list diff:differentiable-multivariate double-float list)
         (values diff:dual &optional))
(defun intersection-equation (coord function threshold shift)
  (declare (optimize (speed 3)))
  (flet ((sub (x1 x2)
           (declare (type diff:dual    x1)
                    (type double-float x2))
           (- x1 x2)))
    (+ (expt (- (funcall function coord) threshold) 2)
       (expt (- (funcall function (mapcar #'sub coord shift)) threshold) 2))))
