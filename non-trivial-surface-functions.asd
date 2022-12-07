(defsystem :non-trivial-surface-functions
  :name :non-trivial-surface-functions
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Computation of surface-surface function for two-dimensional sets"
  :licence "2-clause BSD"
  :serial t
  :pathname "src/"
  :components ((:file "packages")
               (:file "math")
               (:file "surface-surface")
               (:file "discretization"))
  :depends-on (:cl-forward-diff
               :cl-optim
               :serapeum
               :alexandria
               :vp-trees
               :array-operations)
  :in-order-to ((test-op (load-op "non-trivial-surface-functions/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :non-trivial-surface-functions-tests '#:run-tests)))

(defsystem :non-trivial-surface-functions/tests
  :name :non-trivial-surface-functions/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests/"
  :serial t
  :components ((:file "packages")
               (:file "math")
               (:file "tests"))
  :depends-on (:fiveam
               :non-trivial-surface-functions
               :cl-forward-diff
               :serapeum))
