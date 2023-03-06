(defsystem :diff-boundary-corrfn
  :name :diff-boundary-corrfn
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Computation of surface-surface function for two-dimensional sets"
  :licence "2-clause BSD"
  :serial t
  :pathname "src/"
  :components ((:file "packages")
               (:file "math")
               (:file "surface-surface")
               (:file "surface3")
               (:file "discretization"))
  :depends-on (:cl-forward-diff
               :cl-optim
               :serapeum
               :alexandria
               :vp-trees
               :array-operations
               :stateless-iterators)
  :in-order-to ((test-op (load-op "diff-boundary-corrfn/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :diff-boundary-corrfn-tests '#:run-tests)))

(defsystem :diff-boundary-corrfn/tests
  :name :diff-boundary-corrfn/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests/"
  :serial t
  :components ((:file "packages")
               (:file "math")
               (:file "tests"))
  :depends-on (:fiveam
               :diff-boundary-corrfn))
