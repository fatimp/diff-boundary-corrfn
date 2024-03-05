(in-package :diff-boundary-corrfn-tests/math)

(declaim (ftype diff:differentiable-multivariate cube))
(defun cube (coord)
  (declare (optimize (speed 3)))
  (reduce #'max coord :key #'abs))

(declaim (ftype diff:differentiable-multivariate ball))
(defun ball (coord)
  (declare (optimize (speed 3)))
  (sqrt
   (the diff:dual
        (reduce #'+ coord
                :key (lambda (x) (expt x 2))))))

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
