(in-package :diff-boundary-corrfn-tests)

(def-suite surface2 :description "Test F_{ss} function")
(def-suite surface3 :description "Test F_{sss} function")

(defun run-tests ()
  (let ((status (run 'surface2)))
    (explain! status)
    (results-status status)))

(defun run-tests-exhaustive ()
  (every
   (lambda (suite)
     (let ((status (run suite)))
       (explain! status)
       (results-status status)))
   '(surface2 surface3)))

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

(in-suite surface2)

(test square
  (let* ((cf:*ε-threshold* 1d-3)
         (interface (cf:interface #'cf/math:cube 2 2d-1)))
    (is (≈ (cf:surface2 interface '(1d-1 1d-1)) 2))
    (is (≈ (cf:surface2 interface '(5d-1 5d-1)) 0))))

(test diamond
  (let ((cf:*ε-threshold* 1d-3))
    (mapc
     (lambda (scale)
       (is (≈ (cf:surface2 (cf:interface (cf/math:diamond scale) 2  5d-1) '(0d0 1d-1))
              (* 2 (/ (sin (* 2 (atan scale))))))))
     '(7d-1 6d-1 5d-1))))

(test disk
  (let* ((cf:*ε-threshold* 1d-3)
         (interface (cf:interface #'cf/math:ball 2 4d-1)))
    (loop for x from 1d-1 to 7d-1 by 1d-1 do
          (loop for ϕ in (load-time-value
                          (mapcar
                           (alex:curry #'* pi)
                           '(0 1/2 1/4))
                          t)
                do (is (≈ (ss-disk x 4d-1)
                          (cf:surface2 interface (polar->cartesian x ϕ))))))))

(in-suite surface3)

(test cube
  (let* ((cf:*lattice-elements* 500)
         (cf:*ε-threshold* 5d-3)
         (interface (cf:interface #'cf/math:cube 3 2d-1)))
    (is (≈ (cf:surface3 interface '(1d-1 4d-2 3d-2) '(3d-2 3d-2 1d-1)) 2))
    (is (≈ (cf:surface3 interface '(1d-1 4d-2 3d-2) '(3d-2 3d-2 6d-1)) 0))))
