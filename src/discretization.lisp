(in-package :diff-boundary-corrfn)

(sera:-> discretize-field
         (alex:positive-fixnum diff:differentiable-multivariate)
         (values (simple-array double-float (* *)) &optional))
(defun discretize-field (side function)
  "Evaluate a differentiable function (of type
DIFFERENTIABLE-MULTIVARIATE) in points of uniform grid which covers a
square [-1, 1]^2 and return the result as two dimensional array with
dimensions (SIDE SIDE)."
  (declare (optimize (speed 3)))
  (let ((array (make-array (list side side)
                           :element-type 'double-float))
        (Δ (/ (float (1- side) 0d0))))
    (array-operations/utilities:nested-loop (i j)
        (array-dimensions array)
      (setf (aref array i j)
            (diff:dual-realpart
             (funcall function
                      (list
                       (diff:make-dual (1- (* 2 Δ i)) 0d0)
                       (diff:make-dual (1- (* 2 Δ j)) 0d0))))))
    array))
