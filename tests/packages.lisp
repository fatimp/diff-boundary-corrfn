(defpackage diff-boundary-corrfn-tests/math
  (:use #:cl)
  (:local-nicknames (:sera :serapeum)
                    (:diff :cl-forward-diff))
  #.(cl-forward-diff:shadowing-import-math)
  (:export #:square
           #:diamond
           #:disk))

(defpackage diff-boundary-corrfn-tests
  (:use #:cl #:fiveam)
  (:local-nicknames (:sera    :serapeum)
                    (:alex    :alexandria)
                    (:cf      :diff-boundary-corrfn)
                    (:cf/math :diff-boundary-corrfn-tests/math))
  (:export #:run-tests))
