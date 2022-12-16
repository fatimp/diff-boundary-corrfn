(in-package :diff-boundary-corrfn-tests)

(def-suite surface-surface :description "Test surface-surface function")

(defun run-tests ()
  (let ((status (run 'surface-surface)))
    (explain! status)
    (results-status status)))

(defun ≈ (x y)
  (< (abs (- x y)) 1d-4))

(defun polar->cartesian (r ϕ)
  (list (* r (cos ϕ))
        (* r (sin ϕ))))

(sera:-> ss-disk
         ((double-float 0d0)
          (double-float 0d0))
         (values double-float &optional))
(defun ss-disk (dist radius)
  (let ((diameter (* 2 radius)))
    (if (< dist diameter)
        (/ (expt diameter 2)
           (* dist (sqrt (- (expt diameter 2)
                            (expt dist     2)))))
        0d0)))

(in-suite surface-surface)

(test square
  (is (≈ (cf:surface-surface #'cf/math:square 2d-1 '(1d-1 1d-1)) 2))
  (is (≈ (cf:surface-surface #'cf/math:square 2d-1 '(5d-1 5d-1)) 0)))

(test diamond
  (mapc
   (lambda (scale)
     (is (≈ (cf:surface-surface (cf/math:diamond scale) 5d-1 '(0d0 1d-1))
            (* 2 (/ (sin (* 2 (atan scale))))))))
   '(7d-1 6d-1 5d-1)))

(test disk
  (let ((cl-optim:*ε* 1d-5))
    (loop for x from 1d-1 to 7d-1 by 1d-1 do
          (loop for ϕ in (load-time-value
                          (mapcar
                           (alex:curry #'* pi)
                           '(0 1/2 1/4))
                          t)
                do (is (≈ (ss-disk x 4d-1)
                          (cf:surface-surface #'cf/math:disk 4d-1 (polar->cartesian x ϕ))))))))
