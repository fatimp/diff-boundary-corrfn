(in-package :diff-boundary-corrfn-tests/math)

(declaim (ftype diff:differentiable-multivariate cube))
(defun cube (coord)
  (declare (optimize (speed 3)))
  (reduce #'max
          (map '(vector diff:dual) #'abs coord)))

(declaim (ftype diff:differentiable-multivariate ball))
(defun ball (coord)
  (declare (optimize (speed 3)))
  (sqrt
   (the diff:dual
        (reduce #'+ (map '(vector diff:dual)
                         (lambda (x)
                           (expt x 2))
                         coord)))))

(sera:-> diamond
         (double-float)
         (values diff:differentiable-multivariate &optional))
(defun diamond (scale)
  (declare (optimize (speed 3)))
  (lambda (coord)
    (declare (type (simple-array diff:dual) coord))
    (let ((x (aref coord 0))
          (y (aref coord 1)))
      (+ (abs (* scale x))
         (abs y)))))
