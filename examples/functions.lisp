(defpackage example-functions
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria)
                    (#:diff #:cl-forward-diff))
  #.(cl-forward-diff:shadowing-import-math)
  (:export #:gaussian
           #:random-gaussians
           #:gaussian-field
           #:ball))
(in-package :example-functions)

(sera:defconstructor gaussian
  (μ list)
  (σ double-float))

(sera:-> random-gaussians
         (alex:positive-fixnum)
         (values (simple-array gaussian (cl:*)) &optional))
(defun random-gaussians (n)
  "Generate N bell-shaped functions with random peak and sharpness."
  (make-array n
              :element-type 'gaussian
              :initial-contents
              (loop repeat n collect
                    (gaussian (list (cl:- (random 1.5d0) 0.75d0)
                                    (cl:- (random 1.5d0) 0.75d0))
                              (cl:+ 0.1d0 (random 0.3d0))))))

(sera:-> gaussian-field
         ((simple-array gaussian (cl:*)))
         (values diff:differentiable-multivariate &optional))
(defun gaussian-field (gaussians)
  "Return a function which calculates a sum of gaussians defined by
GAUSSIANS at the point (X Y). GAUSSIANS may be generated randomly by
calling RANDOM-GAUSSIANS."
  (declare (optimize (speed 3)))
  (lambda (coord)
    (destructuring-bind (x y) coord
      (declare (type diff:dual x y))
      (reduce
       (lambda (acc gaussian)
         (declare (type diff:dual acc))
         (multiple-value-bind (μ σ)
             (sera:deconstruct gaussian)
           (declare (type double-float σ))
           (destructuring-bind (center-x center-y) μ
             (declare (type double-float center-x center-y))
             (+ acc
                (/
                 (exp (/ (+ (expt (- x center-x) 2)
                            (expt (- y center-y) 2))
                         (* -2 (expt σ 2))))
                 σ)))))
       gaussians :initial-value #d(0 0)))))

(sera:-> ball (list)
         (values diff:dual &optional))
(defun ball (coord)
  (declare (optimize (speed 3))
           (type list coord))
  (sqrt
   (reduce #'+
           (mapcar (lambda (x)
                     (declare (type diff:dual x))
                     (expt x 2))
                   coord))))
