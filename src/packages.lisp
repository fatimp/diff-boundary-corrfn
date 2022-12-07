(defpackage non-trivial-surface-functions/structures
  (:use #:cl)
  (:local-nicknames (:sera :serapeum)
                    (:alex :alexandria))
  (:export #:gaussian
           #:random-gaussians))

(defpackage non-trivial-surface-functions/math
  (:use #:cl)
  (:local-nicknames (:sera      :serapeum)
                    (:diff      :cl-forward-diff)
                    (:sf/struct :non-trivial-surface-functions/structures))
  #.(cl-forward-diff:shadowing-import-math)
  (:export #:gaussian-field
           #:max-metric
           #:l1-metric
           #:intersection-equation))

(defpackage non-trivial-surface-functions
  (:use #:cl)
  (:local-nicknames (:sera      :serapeum)
                    (:diff      :cl-forward-diff)
                    (:alex      :alexandria)
                    (:sf/struct :non-trivial-surface-functions/structures)
                    (:sf/math   :non-trivial-surface-functions/math))
  (:export #:discretize-field
           #:intersections
           #:surface-surface))
