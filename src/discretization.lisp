(in-package :diff-boundary-corrfn)

(sera:-> discretize-field
         (alex:positive-fixnum
          alex:positive-fixnum
          diff:differentiable-multivariate)
         (values (simple-array double-float) &optional))
(defun discretize-field (side ndims function)
  "Evaluate a differentiable function (of type
DIFFERENTIABLE-MULTIVARIATE) in points of uniform grid which covers a
n-dimensional cube [-1, 1]^NDIMS and return the result as
NDIMS-dimensional array with dimensions (SIDE SIDE … SIDE)."
  (declare (optimize (speed 3)))
  (let ((array (make-array (loop repeat ndims collect side)
                           :element-type 'double-float))
        (indices (si:imap #'alex:flatten
                          (reduce #'si:product
                                  (loop repeat ndims collect (si:range 0 side)))))
        (Δ (/ (float (1- side) 0d0))))
    (si:do-iterator (index indices)
      (setf (apply #'aref array index)
            (diff:dual-realpart
             (funcall function
                    (mapcar (lambda (i)
                              (declare (type alex:non-negative-fixnum i))
                              (diff:make-dual (1- (* 2 Δ i))))
                            index)))))
    array))
