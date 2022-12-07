(in-package :non-trivial-surface-functions/structures)

(sera:defconstructor gaussian
  (μ list)
  (σ single-float))

(sera:-> random-gaussians
         (alex:positive-fixnum)
         (values (simple-array gaussian (*)) &optional))
(defun random-gaussians (n)
  (make-array n
              :element-type 'gaussian
              :initial-contents
              (loop repeat n collect
                    (gaussian (list (- (random 1.0) 0.5)
                                    (- (random 1.0) 0.5))
                              (+ 0.1 (random 0.3))))))
