(defpackage diff-boundary-corrfn-tests/math
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:diff #:cl-forward-diff))
  #.(cl-forward-diff:shadowing-import-math)
  (:export #:cube
           #:diamond
           #:ball))

(defpackage diff-boundary-corrfn-tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:sera    #:serapeum)
                    (#:alex    #:alexandria)
                    (#:diff    #:cl-forward-diff)
                    (#:cf      #:diff-boundary-corrfn)
                    (#:cf/math #:diff-boundary-corrfn-tests/math))
  (:export #:run-tests
           #:run-tests-exhaustive))
