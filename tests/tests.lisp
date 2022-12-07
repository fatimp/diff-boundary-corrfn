(in-package :non-trivial-surface-functions-tests)

(def-suite surface-surface :description "Test surface-surface function")

(defun run-tests ()
  (let ((status (run 'surface-surface)))
    (explain! status)
    (results-status status)))

(defun ≈ (x y)
  (< (abs (- x y)) 1f-4))

(in-suite surface-surface)

(test square
  (is (≈ (sf:surface-surface #'sf/math:square 0.2 '(0.1 0.1)) 2))
  (is (≈ (sf:surface-surface #'sf/math:square 0.2 '(0.5 0.5)) 0)))

(test diamond
  (mapc
   (lambda (scale)
     (is (≈ (sf:surface-surface (sf/math:diamond scale) 0.5 '(0.0 0.1))
            (* 2 (/ (sin (* 2 (atan scale))))))))
   '(0.7 0.6 0.5)))
