(defpackage diff-boundary-corrfn/math
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria)
                    (#:diff #:cl-forward-diff))
  #.(cl-forward-diff:shadowing-import-math)
  (:export #:intersection-equation
           #:intersection-equation-3))

(defpackage diff-boundary-corrfn
  (:use #:cl)
  (:local-nicknames (#:sera    #:serapeum)
                    (#:diff    #:cl-forward-diff)
                    (#:alex    #:alexandria)
                    (#:cf/math #:diff-boundary-corrfn/math)
                    (#:si      #:stateless-iterators))
  (:export #:discretize-field
           #:%interface
           #:interface
           #:intersections2
           #:intersections3
           #:surface2
           #:surface2-at-dist
           #:surface3

           #:*lattice-elements*
           #:*ε-pixels*
           #:*ε-threshold*
           #:*ε-intersections*

           #:dimensionality-error))
