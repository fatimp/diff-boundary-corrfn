(defun do-all()
  (ql:quickload :diff-boundary-corrfn/tests)
  (uiop:quit
   (if (uiop:call-function "diff-boundary-corrfn-tests:run-tests")
       0 1)))

(do-all)
