(in-package :diff-boundary-corrfn)

(declaim (type alex:positive-fixnum *ε-pixels*)
         (type (double-float 0d0) *ε-intersections*))

(defparameter *ε-pixels* 1
  "Parameter used when searching for candidate points for an
intersection of the interfaces. Higher value results in more
candidates.")
(defparameter *ε-intersections* 1d-3
  "Parameter used when searching for unique points when the interface
intersects its shifted self. Solutions which are closer to each
other than this parameter are considered duplicates.")

(sera:-> intersection-candidates-2 (%interface (simple-array double-float (*)))
         (values list &optional))
(defun intersection-candidates-2 (interface shift)
  "Return a list of points X where it's possible that INTERFACE
intersects itself shifted by SHIFT."
  (declare (optimize (speed 3)))
  (check-dimensionality 2 interface shift)
  (let* ((interface-tree (%interface-tree interface))
         (interface-points (vp-trees:flatten interface-tree))
         intersection-candidates)
    (dolist (interface-point interface-points)
      (declare (type (simple-array double-float (*)) interface-point))
      (when (vp-trees:items-in-ball interface-tree
                                    (map '(vector double-float) #'- interface-point shift)
                                    (/ (* 2d0 *ε-pixels*) *lattice-elements*)
                                    #'euclidean-metric)
        (push interface-point intersection-candidates)))
    intersection-candidates))

(sera:-> intersections2 (%interface (simple-array double-float (*)))
         (values list &optional))
(defun intersections2 (interface shift)
  "Return a list of exact intersections of the interface (a solution
of the equation f(X) = T) with its shifted self. INTERFACE is a
precomputed object returned by INTERFACE function."
  (declare (optimize (speed 3)))
  (let ((candidates (intersection-candidates-2 interface shift)))
    (remove-duplicates
     (mapcar
      (lambda (candidate)
        (cl-optim:bfgs
         (alex:rcurry #'cf/math:intersection-equation
                      (%interface-function  interface)
                      (%interface-threshold interface)
                      shift)
         candidate
         ;; cl-optim:*ε* must be << *ε-intersections*
         :ε (* *ε-intersections* 1d-2)))
     candidates)
     :test (lambda (p1 p2)
             (< (euclidean-metric p1 p2) *ε-intersections*)))))

(sera:-> jacobian (&rest (simple-array double-float (*)))
         (values double-float &optional))
(defun jacobian (&rest points)
  (let* ((length (length points))
         (normalized (mapcar
                      (lambda (vector)
                        (magicl:normalize
                         (magicl:from-array vector (list length 1))))
                      points))
         (matrix (magicl:hstack normalized)))
    (/ (abs (magicl:det matrix)))))

(sera:-> surface2 (%interface (simple-array double-float (*)))
         (values double-float &optional))
(defun surface2 (interface shift)
  "Calculate the surface-surface function at the point SHIFT for a set
with differentiable boundary f(x, y) = T. Boundary must be a
precomputed object returned by INTERFACE function.

NB: Intersection points of the boundary and its shifted self must be
in a square [-1, 1]^2."
  (let ((intersections (intersections2 interface shift))
        (function (%interface-function interface)))
    (reduce #'+ intersections
            :key (lambda (intersection)
                   (abs (jacobian
                         (cl-forward-diff:ad-multivariate
                          function intersection)
                         (cl-forward-diff:ad-multivariate
                          function (map '(vector double-float)
                                        #'- intersection shift)))))
            :initial-value 0d0)))

(sera:-> surface2-at-dist
         (%interface (double-float 0d0) alex:positive-fixnum)
         (values list &optional))
(defun surface2-at-dist (interface dist n)
  "Calculate surface-surface function at N points X = (x1, x2) which
have |X| = DIST. These points cover an arc with angles from 0 to π."
  (flet ((polar->cartesian (ϕ)
           (diff:to-doubles
            (list (* dist (cos ϕ))
                  (* dist (sin ϕ))))))
    (loop with delta = (/ pi n)
          for i below n
          for ϕ     = (* i delta)
          for coord = (polar->cartesian ϕ) collect
          (cons (surface2 interface coord) coord))))
