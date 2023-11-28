(in-package :diff-boundary-corrfn)

(sera:-> intersection-candidates-3
         (%interface
          (simple-array double-float (*))
          (simple-array double-float (*)))
         (values list &optional))
(defun intersection-candidates-3 (interface shift1 shift2)
  "Return a list of points X where it's possible that INTERFACE
intersects itself shifted by SHIFT1 and SHIFT2."
  (declare (optimize (speed 3)))
  (check-dimensionality 3 interface shift1 shift2)
  (let* ((interface-tree (%interface-tree interface))
         (interface-points (vp-trees:flatten interface-tree))
         intersection-candidates)
    (dolist (interface-point interface-points)
      (declare (type (simple-array double-float (*)) interface-point))
      (when (every
             (lambda (shift)
               (declare (type (simple-array double-float (*)) shift))
               (vp-trees:items-in-ball interface-tree
                                       (map '(vector double-float)
                                            #'- interface-point shift)
                                       (/ (* 2d0 *ε-pixels*) *lattice-elements*)
                                       #'euclidean-metric))
             (list shift1 shift2))
        (push interface-point intersection-candidates)))
  intersection-candidates))

(sera:-> intersections3
         (%interface
          (simple-array double-float (*))
          (simple-array double-float (*)))                         
         (values list &optional))
(defun intersections3 (interface shift1 shift2)
  "Return a list of exact intersections of the interface (a solution
of the equation f(X) = T) with its shifted selves. INTERFACE is a
precomputed object returned by INTERFACE function."
  (declare (optimize (speed 3)))
  (let ((candidates (intersection-candidates-3 interface shift1 shift2))
        (target (alex:rcurry #'cf/math:intersection-equation-3
                             (%interface-function  interface)
                             (%interface-threshold interface)
                             shift1 shift2))
        intersections)
    (dolist (candidate candidates)
      (let ((solution
             (cl-optim:bfgs target candidate
                            ;; cl-optim:*ε* must be << *ε-intersections*
                            :ε (* *ε-intersections* 1d-2))))
        (when (< (diff:dual-realpart (funcall target (map '(vector diff:dual)
                                                          #'diff:make-dual solution)))
                 ;; FIXME: make a parameter?
                 1d-5)
          (push solution intersections))))
    (remove-duplicates intersections
                       :test (lambda (p1 p2)
                               (< (euclidean-metric p1 p2)
                                  *ε-intersections*)))))

(sera:-> surface3
         (%interface
          (simple-array double-float (*))
          (simple-array double-float (*)))
         (values double-float &optional))
(defun surface3 (interface shift1 shift2)
  "Calculate F_{sss} function at the point (SHIFT1, SHIFT2) for a set
with differentiable boundary f(x, y, z) = T. Boundary must be a
precomputed object returned by INTERFACE function.

NB: Intersection points of the boundary and its shifted self must be
in a square [-1, 1]^3."
  (let ((intersections (intersections3 interface shift1 shift2))
        (function (%interface-function interface)))
    (reduce
     #'+
     (mapcar
      (lambda (intersection)
        (let ((gradient-here     (diff:ad-multivariate function intersection))
              (gradient-shifted1 (diff:ad-multivariate
                                  function (map '(vector double-float)
                                                #'- intersection shift1)))
              (gradient-shifted2 (diff:ad-multivariate
                                  function (map '(vector double-float)
                                                #'- intersection shift2))))
          (jacobian gradient-here
                    gradient-shifted1
                    gradient-shifted2)))
      intersections)
     :initial-value 0d0)))
