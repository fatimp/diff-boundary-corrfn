(in-package :diff-boundary-corrfn-tests)

(def-suite surface-surface :description "Test surface-surface function")

(defun run-tests ()
  (let ((status (run 'surface-surface)))
    (explain! status)
    (results-status status)))

(defun ≈ (x y)
  (< (abs (- x y)) 1f-4))

(in-suite surface-surface)

(test square
  (is (≈ (cf:surface-surface #'cf/math:square 0.2 '(0.1 0.1)) 2))
  (is (≈ (cf:surface-surface #'cf/math:square 0.2 '(0.5 0.5)) 0)))

(test diamond
  (mapc
   (lambda (scale)
     (is (≈ (cf:surface-surface (cf/math:diamond scale) 0.5 '(0.0 0.1))
            (* 2 (/ (sin (* 2 (atan scale))))))))
   '(0.7 0.6 0.5)))
