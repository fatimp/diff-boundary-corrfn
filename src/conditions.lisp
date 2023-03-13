(in-package :diff-boundary-corrfn)

(define-condition dimensionality-error (error)
  ((expected-ndims :type    alex:positive-fixnum
                   :reader  expected-ndims
                   :initarg :expected)
   (actual-ndims   :type    alex:positive-fixnum
                   :reader  actual-ndims
                   :initarg :actual))
  (:report
   (lambda (c s)
     (format s "Dimensionality mismatch: expected ~d, actual ~d"
             (expected-ndims c)
             (actual-ndims   c)))))
