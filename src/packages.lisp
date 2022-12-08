(defpackage diff-boundary-corrfn/math
  (:use #:cl)
  (:local-nicknames (:sera :serapeum)
                    (:alex :alexandria)
                    (:diff :cl-forward-diff))
  #.(cl-forward-diff:shadowing-import-math)
  (:export #:gaussian
           #:random-gaussians
           #:gaussian-field
           #:intersection-equation))

(defpackage diff-boundary-corrfn
  (:use #:cl)
  (:local-nicknames (:sera    :serapeum)
                    (:diff    :cl-forward-diff)
                    (:alex    :alexandria)
                    (:cf/math :diff-boundary-corrfn/math))
  (:export #:discretize-field
           #:intersections
           #:surface-surface
           #:*lattice-elements*
           #:*ε-pixels*
           #:*ε-threshold*
           #:*ε-intersections*))
