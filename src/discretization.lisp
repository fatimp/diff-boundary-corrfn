(in-package :non-trivial-surface-functions)

(sera:-> discretize-field
         (alex:positive-fixnum diff:differentiable-multivariate)
         (values (simple-array single-float (* *)) &optional))
(defun discretize-field (side function)
  (declare (optimize (speed 3)))
  (let ((array (make-array (list side side)
                           :element-type 'single-float))
        (Δ (/ (float (1- side)))))
    (array-operations/utilities:nested-loop (i j)
        (array-dimensions array)
      (setf (aref array i j)
            (diff:dual-realpart
             (funcall function
                      (list
                       (diff:dual (1- (* 2 Δ i)) 0.0)
                       (diff:dual (1- (* 2 Δ j)) 0.0))))))
    array))
