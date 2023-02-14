(in-package :diff-boundary-corrfn)

(declaim (type alex:positive-fixnum *lattice-elements* *ε-pixels*)
         (type (double-float 0d0) *ε-threshold* *ε-intersections*))

(defparameter *lattice-elements* 1000
  "Parameter used when searching for candidate points for the
interface. Higher value gives more candidates.")
(defparameter *ε-pixels* 1
  "Parameter used when searching for candidate points for an
intersection of the interfaces. Higher value results in more
candidates.")
(defparameter *ε-threshold* 4d-2
  "Parameter used when searching for candidate points for the
interface. Higher value gives more candidates.")
(defparameter *ε-intersections* 1d-3
  "Parameter used when searching for unique points when the interface
intersects its shifted self. Solutions which are closer to each
other than this parameter are considered duplicates.")

(sera:-> interface-candidates
         (diff:differentiable-multivariate double-float)
         (values list &optional))
(defun interface-candidates (function threshold)
  "Return a list of points (X Y) where FUNCTION(X, Y) ≈ THRESHOLD."
  (declare (optimize (speed 3)))
  (let ((Δ (/ (float (1- *lattice-elements*) 0d0)))
        candidates)
    (loop for i below *lattice-elements*
          for x = (1- (* 2 Δ i)) do
          (loop for j below *lattice-elements*
                for y = (1- (* 2 Δ j)) do
                (when (< (abs (- threshold
                                 (diff:dual-realpart
                                  (funcall function
                                           (list
                                            (diff:make-dual x 0d0)
                                            (diff:make-dual y 0d0))))))
                         *ε-threshold*)
                  (push (list x y) candidates))))
    candidates))

(sera:-> euclidean-metric
         (list list)
         (values double-float &optional))
(defun euclidean-metric (p1 p2)
  (declare (optimize (speed 3)))
  (let ((x1 (first p1))
        (y1 (second p1))
        (x2 (first p2))
        (y2 (second p2)))
    (declare (type double-float x1 x2 y1 y2))
    (sqrt (+ (expt (- x1 x2) 2)
             (expt (- y1 y2) 2)))))

(sera:-> intersection-candidates
         (diff:differentiable-multivariate double-float list)
         (values list &optional))
(defun intersection-candidates (function threshold shift)
  "Return a list of points X where f(X) ≈ THRESHOLD and f(X - SHIFT) ≈
THRESHOLD."
  (declare (optimize (speed 3)))
  (let* ((interface-candidates
          (interface-candidates function threshold))
         (interface-candidates-tree
          (vp-trees:make-vp-tree interface-candidates
                                 #'euclidean-metric))
         intersection-candidates)
    (flet ((sub (x1 x2)
             (declare (type double-float x1 x2))
             (- x1 x2)))
      (dolist (interface-candidate interface-candidates)
        (when (vp-trees:items-in-ball interface-candidates-tree
                                      (mapcar #'sub interface-candidate shift)
                                      (/ (* 2 *ε-pixels*) *lattice-elements*)
                                      #'euclidean-metric)
          (push interface-candidate intersection-candidates))))
    intersection-candidates))

(sera:-> intersections
         (diff:differentiable-multivariate double-float list)
         (values list &optional))
(defun intersections (function threshold shift)
  "Return a list of exact intersections of the interface (a solution
of the equation f(X) = THRESHOLD) with its shifted self."
  (declare (optimize (speed 3)))
  (let ((candidates (intersection-candidates function threshold shift)))
    (remove-duplicates
     (mapcar
      (lambda (candidate)
        (cl-optim:adam
         (alex:rcurry #'cf/math:intersection-equation function threshold shift)
         candidate :η 1d-2))
     candidates)
     :test (lambda (p1 p2)
             (< (euclidean-metric p1 p2) *ε-intersections*)))))

(defun normalize (vector)
  (let ((norm (euclidean-metric vector '(0d0 0d0))))
    (mapcar (alex:rcurry #'/ norm) vector)))

(defun dot (v1 v2)
  (reduce #'+ (mapcar #'* v1 v2)))

(sera:-> surface-surface
         (diff:differentiable-multivariate double-float list)
         (values double-float &optional))
(defun surface-surface (function threshold shift)
  "Calculate the surface-surface function at the point SHIFT for a set
with differentiable boundary FUNCTION(X, Y) = THRESHOLD.

NB: Intersection points of the boundary and its shifted self must be
in a square [-1, 1]^2."
  (let ((intersections (intersections function threshold shift)))
    (reduce
     #'+
     (mapcar
      (lambda (intersection)
        (let ((gradient-here    (normalize (cl-forward-diff:ad-multivariate function intersection)))
              (gradient-shifted (normalize (cl-forward-diff:ad-multivariate
                                            function (mapcar #'- intersection shift)))))
          (/ (sqrt (- 1 (expt (dot gradient-here gradient-shifted) 2))))))
      intersections)
     :initial-value 0d0)))

(sera:-> surface-surface-at-dist
         (diff:differentiable-multivariate
          double-float
          (double-float 0d0)
          alex:positive-fixnum)
         (values list &optional))
(defun surface-surface-at-dist (function threshold dist n)
  "Calculate surface-surface function at N points X = (x1, x2) which
have |X| = DIST. These points cover an arc with angles from 0 to π."
  (flet ((polar->cartesian (ϕ)
           (list (* dist (cos ϕ))
                 (* dist (sin ϕ)))))
    (loop with delta = (/ pi n)
          for i below n
          for ϕ     = (* i delta)
          for coord = (polar->cartesian ϕ) collect
          (cons (surface-surface function threshold coord)
                coord))))
