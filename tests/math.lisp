(in-package :non-trivial-surface-functions-tests/math)

(declaim (ftype diff:differentiable-multivariate square))
(defun square (coord)
  (declare (optimize (speed 3)))
  (destructuring-bind (x y) coord
    (declare (type diff:dual x y))
    (max (abs x)
         (abs y))))

(sera:-> diamond
         (single-float)
         (values diff:differentiable-multivariate &optional))
(defun diamond (scale)
  (declare (optimize (speed 3))
           (type single-float scale))
  (lambda (coord)
    (destructuring-bind (x y) coord
      (declare (type diff:dual x y))
      (+ (abs (* scale x))
         (abs y)))))
