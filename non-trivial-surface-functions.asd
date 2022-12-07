(defsystem :non-trivial-surface-functions
  :name :non-trivial-surface-functions
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Automatic differentiation system (forward mode)"
  :licence "2-clause BSD"
  :serial t
  :pathname "src/"
  :components ((:file "packages")
               (:file "structures")
               (:file "medium")
               (:file "surface-surface")
               (:file "discretization"))
  :depends-on (:cl-forward-diff
               :cl-optim
               :serapeum
               :alexandria
               :vp-trees
               :array-operations))
