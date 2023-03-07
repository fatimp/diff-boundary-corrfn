(in-package :diff-boundary-corrfn)

(sera:-> intersection-candidates-3 (%interface list list)
         (values list &optional))
(defun intersection-candidates-3 (interface shift1 shift2)
  "Return a list of points X where it's possible that INTERFACE
intersects itself shifted by SHIFT1 and SHIFT2."
  (declare (optimize (speed 3)))
  (let* ((interface-tree (%interface-tree interface))
         (interface-points (vp-trees:flatten interface-tree))
         intersection-candidates)
    (flet ((sub (x1 x2)
             (declare (type double-float x1 x2))
             (- x1 x2)))
      (dolist (interface-point interface-points)
        (when (every
               (lambda (shift)
                 (vp-trees:items-in-ball interface-tree
                                         (mapcar #'sub interface-point shift)
                                         (/ (* 2d0 *ε-pixels*) *lattice-elements*)
                                         #'euclidean-metric))
               (list shift1 shift2))
          (push interface-point intersection-candidates))))
    intersection-candidates))

(sera:-> intersections3 (%interface list list)
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
             (cl-optim:adam target candidate
                            :η 1d-2
                            ;; cl-optim:*ε* must be << *ε-intersections*
                            :ε (* *ε-intersections* 1d-2))))
        (when (< (diff:dual-realpart (funcall target (mapcar #'diff:make-dual solution)))
                 ;; FIXME: make a parameter?
                 1d-5)
          (push solution intersections))))
    (remove-duplicates intersections
                       :test (lambda (p1 p2)
                               (< (euclidean-metric p1 p2)
                                  *ε-intersections*)))))

(sera:-> area3 (list list list)
         (values double-float &optional))
(defun area3 (v1 v2 v3)
  (let ((matrix (magicl:from-list
                 (append v1 v2 v3)
                 '(3 3))))
    (/ (abs (magicl:det matrix)))))

(sera:-> surface3 (%interface list list)
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
        (let ((gradient-here     (normalize (diff:ad-multivariate function intersection)))
              (gradient-shifted1 (normalize (diff:ad-multivariate
                                             function (mapcar #'- intersection shift1))))
              (gradient-shifted2 (normalize (diff:ad-multivariate
                                             function (mapcar #'- intersection shift2)))))
          (area3 gradient-here
                 gradient-shifted1
                 gradient-shifted2)))
      intersections)
     :initial-value 0d0)))
