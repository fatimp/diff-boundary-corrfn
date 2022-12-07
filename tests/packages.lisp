(defpackage non-trivial-surface-functions-tests/math
  (:use #:cl)
  (:local-nicknames (:sera :serapeum)
                    (:diff :cl-forward-diff))
  #.(cl-forward-diff:shadowing-import-math)
  (:export #:square
           #:diamond))

(defpackage non-trivial-surface-functions-tests
  (:use #:cl #:fiveam)
  (:local-nicknames (:sera      :serapeum)
                    (:sf        :non-trivial-surface-functions)
                    (:sf/math   :non-trivial-surface-functions-tests/math))
  (:export #:run-tests))
