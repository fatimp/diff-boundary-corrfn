(defpackage non-trivial-surface-functions/math
  (:use #:cl)
  (:local-nicknames (:sera :serapeum)
                    (:alex :alexandria)
                    (:diff :cl-forward-diff))
  #.(cl-forward-diff:shadowing-import-math)
  (:export #:gaussian
           #:random-gaussians
           #:gaussian-field
           #:intersection-equation))

(defpackage non-trivial-surface-functions
  (:use #:cl)
  (:local-nicknames (:sera      :serapeum)
                    (:diff      :cl-forward-diff)
                    (:alex      :alexandria)
                    (:sf/math   :non-trivial-surface-functions/math))
  (:export #:discretize-field
           #:intersections
           #:surface-surface))
