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

(sera:-> euclidean-metric
         (list list)
         (values double-float &optional))
(defun euclidean-metric (p1 p2)
  (declare (optimize (speed 3)))
  (sqrt
   (reduce
    #'+
    (map '(vector double-float)
         (lambda (x1 x2)
           (declare (type double-float x1 x2))
           (expt (- x1 x2) 2))
         p1 p2))))

(sera:defconstructor %interface
  (function  diff:differentiable-multivariate)
  (threshold double-float)
  (tree      vp-trees:vp-node))

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
                    (mapcar
                     (lambda (i)
                       (declare (type alex:non-negative-fixnum i))
                       (1- (* 2 Δ i)))
                     (alex:flatten coords)))
                  (reduce #'si:product
                          (loop repeat ndims collect (si:range 0 *lattice-elements*)))))
         candidates)
    (si:do-iterator (coord coords)
      (when (< (abs (- threshold
                       (diff:dual-realpart
                        (funcall function (mapcar #'diff:make-dual coord)))))
               *ε-threshold*)
        (push coord candidates)))
    (%interface function threshold
                (vp-trees:make-vp-tree candidates #'euclidean-metric))))

(sera:-> intersection-candidates (%interface list)
         (values list &optional))
(defun intersection-candidates (interface shift)
  "Return a list of points X where it's possible that INTERFACE
intersects itself shifted by SHIFT."
  (declare (optimize (speed 3)))
  (let* ((interface-tree (%interface-tree interface))
         (interface-points (vp-trees:flatten interface-tree))
         intersection-candidates)
    (flet ((sub (x1 x2)
             (declare (type double-float x1 x2))
             (- x1 x2)))
      (dolist (interface-point interface-points)
        (when (vp-trees:items-in-ball interface-tree
                                      (mapcar #'sub interface-point shift)
                                      (/ (* 2 *ε-pixels*) *lattice-elements*)
                                      #'euclidean-metric)
          (push interface-point intersection-candidates))))
    intersection-candidates))

(sera:-> intersections (%interface list)
         (values list &optional))
(defun intersections (interface shift)
  "Return a list of exact intersections of the interface (a solution
of the equation f(X) = T) with its shifted self. INTERFACE is a
precomputed object returned by INTERFACE function."
  (declare (optimize (speed 3)))
  (let ((candidates (intersection-candidates interface shift)))
    (remove-duplicates
     (mapcar
      (lambda (candidate)
        (cl-optim:adam
         (alex:rcurry #'cf/math:intersection-equation
                      (%interface-function  interface)
                      (%interface-threshold interface)
                      shift)
         candidate
         :η 1d-2
         ;; cl-optim:*ε* must be << *ε-intersections*
         :ε (* *ε-intersections* 1d-2)))
     candidates)
     :test (lambda (p1 p2)
             (< (euclidean-metric p1 p2) *ε-intersections*)))))

(sera:-> normalize (list)
         (values list &optional))
(defun normalize (list)
  (let ((norm (euclidean-metric
               list
               (loop repeat (length list) collect 0d0))))
    (mapcar (alex:rcurry #'/ norm) list)))

(defun dot (v1 v2)
  (reduce #'+ (mapcar #'* v1 v2)))

(sera:-> surface-surface (%interface list)
         (values double-float &optional))
(defun surface-surface (interface shift)
  "Calculate the surface-surface function at the point SHIFT for a set
with differentiable boundary f(x, y) = T. Boundary must be a
precomputed object returned by INTERFACE function.

NB: Intersection points of the boundary and its shifted self must be
in a square [-1, 1]^2."
  (let ((intersections (intersections interface shift))
        (function (%interface-function interface)))
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
         (%interface (double-float 0d0) alex:positive-fixnum)
         (values list &optional))
(defun surface-surface-at-dist (interface dist n)
  "Calculate surface-surface function at N points X = (x1, x2) which
have |X| = DIST. These points cover an arc with angles from 0 to π."
  (flet ((polar->cartesian (ϕ)
           (list (* dist (cos ϕ))
                 (* dist (sin ϕ)))))
    (loop with delta = (/ pi n)
          for i below n
          for ϕ     = (* i delta)
          for coord = (polar->cartesian ϕ) collect
          (cons (surface-surface interface coord) coord))))
