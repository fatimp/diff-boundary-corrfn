(in-package :diff-boundary-corrfn)

(declaim (type alex:positive-fixnum *lattice-elements*)
         (type (double-float 0d0) *ε-threshold*))
(defparameter *lattice-elements* 1000
  "Parameter used when searching for candidate points for the
interface. Higher value gives more candidates.")
(defparameter *ε-threshold* 4d-2
  "Parameter used when searching for candidate points for the
interface. Higher value gives more candidates.")

(sera:-> euclidean-metric
         ((simple-array double-float (*))
          (simple-array double-float (*)))
         (values (double-float 0d0) &optional))
(defun euclidean-metric (p1 p2)
  (declare (optimize (speed 3)))
  (sqrt
   (reduce
    #'+
    (map '(vector double-float)
         (lambda (x1 x2)
           (expt (- x1 x2) 2))
         p1 p2)
    :initial-value 0d0)))

(sera:defconstructor %interface
  (function  diff:differentiable-multivariate)
  (threshold double-float)
  (ndims     alex:positive-fixnum)
  (tree      vp-trees:vp-node))

(sera:-> check-dimensionality
         (alex:positive-fixnum %interface &rest (simple-array double-float (*)))
         (values &optional))
(defun check-dimensionality (expected interface &rest shifts)
  (flet ((%signal (actual)
           (error 'dimensionality-error
                  :expected expected
                  :actual actual)))
    (let ((ndims (%interface-ndims interface)))
      (unless (= ndims expected)
        (%signal ndims)))

    (let ((mismatch (find-if
                     (lambda (shift)
                       (/= (length shift) expected))
                     shifts)))
      (when mismatch
        (%signal (length mismatch)))))
  (values))

(sera:-> interface
         (diff:differentiable-multivariate
          alex:positive-fixnum
          double-float)
         (values %interface &optional))
(defun interface (function ndims threshold)
  "Calculate interface of a set FUNCTION(COORD) < THRESHOLD where
COORD is a list with NDIMS elements. Result of this function is
accepted by SURFACE-SURFACE functions and its siblings."
  (declare (optimize (speed 3)))
  (let* ((Δ (/ (float (1- *lattice-elements*) 0d0)))
         (coords (si:imap
                  (lambda (coords)
                    (map '(vector double-float)
                         (lambda (i)
                           (declare (type alex:non-negative-fixnum i))
                           (1- (* 2 Δ i)))
                         coords))
                  (si:power (si:range 0 *lattice-elements*) ndims)))
         candidates)
    (si:do-iterator (coord coords)
      (when (< (abs (- threshold
                       (diff:dual-realpart
                        (funcall function (map '(vector diff:dual)
                                               #'diff:make-dual
                                               coord)))))
               *ε-threshold*)
        (push coord candidates)))
    (%interface function threshold ndims
                (vp-trees:make-vp-tree candidates #'euclidean-metric))))

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
        (indices (si:power (si:range 0 side) ndims))
        (Δ (/ (float (1- side) 0d0))))
    (si:do-iterator (index indices)
      (setf (apply #'aref array index)
            (diff:dual-realpart
             (funcall function
                    (map
                     '(vector diff:dual)
                     (lambda (i)
                       (declare (type alex:non-negative-fixnum i))
                       (diff:make-dual (1- (* 2 Δ i))))
                     index)))))
    array))
