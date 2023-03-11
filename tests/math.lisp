(in-package :diff-boundary-corrfn-tests/math)

(declaim (ftype diff:differentiable-multivariate cube))
(defun cube (coord)
  (declare (optimize (speed 3)))
  (reduce #'max (mapcar
                 (lambda (x)
                   (declare (type diff:dual x))
                   (abs x))
                 coord)))

(declaim (ftype diff:differentiable-multivariate ball))
(defun ball (coord)
  (declare (optimize (speed 3)))
  (sqrt
   (the diff:dual
        (reduce #'+ (mapcar
                     (lambda (x)
                       (declare (type diff:dual x))
                       (expt x 2))
                     coord)))))

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
