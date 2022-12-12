(in-package :diff-boundary-corrfn-tests)

(def-suite surface-surface :description "Test surface-surface function")

(defun run-tests ()
  (let ((status (run 'surface-surface)))
    (explain! status)
    (results-status status)))

(defun ≈ (x y)
  (< (abs (- x y)) 1f-4))

(defun polar->cartesian (r ϕ)
  (list (* r (cos ϕ))
        (* r (sin ϕ))))

(sera:-> ss-disk
         ((single-float 0.0)
          (single-float 0.0))
         (values single-float &optional))
(defun ss-disk (dist radius)
  (let ((diameter (* 2 radius)))
    (if (< dist diameter)
        (/ (expt diameter 2)
           (* dist (sqrt (- (expt diameter 2)
                            (expt dist     2)))))
        0.0)))

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

(test disk
  (let ((cl-optim:*ε* 1f-5))
    (loop for x from 0.1 to 0.7 by 0.1 do
          (loop for ϕ in (load-time-value
                          (mapcar
                           (alex:curry #'* (float pi 0.0))
                           '(0 1/2 1/4))
                          t)
                do (is (≈ (ss-disk x 0.4)
                          (cf:surface-surface #'cf/math:disk 0.4 (polar->cartesian x ϕ))))))))
