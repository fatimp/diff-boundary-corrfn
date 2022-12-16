(in-package :diff-boundary-corrfn)

(sera:-> discretize-field
         (alex:positive-fixnum diff:differentiable-multivariate)
         (values (simple-array double-float (* *)) &optional))
(defun discretize-field (side function)
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
