(in-package :diff-boundary-corrfn-tests/math)

(declaim (ftype diff:differentiable-multivariate square))
(defun square (coord)
  (declare (optimize (speed 3)))
  (destructuring-bind (x y) coord
    (declare (type diff:dual x y))
    (max (abs x)
         (abs y))))

(declaim (ftype diff:differentiable-multivariate disk))
(defun disk (coord)
  (declare (optimize (speed 3)))
  (destructuring-bind (x y) coord
    (declare (type diff:dual x y))
    (sqrt
     (+ (expt x 2)
        (expt y 2)))))

(sera:-> diamond
         (double-float)
         (values diff:differentiable-multivariate &optional))
(defun diamond (scale)
  (declare (optimize (speed 3)))
  (lambda (coord)
    (destructuring-bind (x y) coord
      (declare (type diff:dual x y))
      (+ (abs (* scale x))
         (abs y)))))
