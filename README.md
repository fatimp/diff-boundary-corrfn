# diff-boundary-corrfn
[![CI tests](https://github.com/fatimp/diff-boundary-corrfn/actions/workflows/test.yml/badge.svg)](https://github.com/fatimp/diff-boundary-corrfn/actions/workflows/test.yml)

This library provides a function which calculates surface-surface
(resp. surface-surface-surface) correlation functions (see Salvatore Torquato's
book "Random heterogeneous materials") for a two- (resp. three-)dimensional sets
with differentiable (almost everywhere) boundary. A set is defined as points
which satisfy an inequation `f(X) > T` where `X` is a two- or three-dimensional
vector.  The function `f` must be defined using `cl-forward-diff` package and
have `cl-forward-diff:differentiable-multivariate` type. Let's look at this
example:

``` lisp
(defpackage math-function
  (:use #:cl)
  (:local-nicknames (:diff :cl-forward-diff))
  #.(cl-forward-diff:shadowing-import-math)
  (:export #:f))
(in-package :math-function)

(declaim (ftype diff:differentiable-multivariate f))
(defun f (coord)
  (declare (optimize (speed 3)))
  (let ((x (aref coord 0))
        (y (aref coord 1)))
    (max (abs x)
         (abs y))))
```

An inequation `(> (FUNCALL F COORD) 2d-1)` defines a set `ℝ \ [-0.2, 0.2]^2` with
a boundary which is differentiable almost everywhere. Now evaluation of

``` lisp
(diff-boundary-corrfn:surface2
 (diff-boundary-corrfn:interface
  #'math-function:f
  2            ;; Number of dimensions
  2d-1)        ;; Parameter T
 (cl-forward-diff:to-doubles '(1d-1 1d-1))) ;; Calculate F_{ss}(0.1, 0.1)
```

gives `2d0`. This can be understood as that two boundaries of a square which lie
close to each other have two points of intersection and have an angle of
intersection equal to `π/2`:

~~~~
|-------|
|  |----|--|
|  |    |  |
|  |    |  |
|--|----|  |
   |-------|
~~~~

## Caveats

Currently all points of intersection must lie in a cube `[-1, 1]^{ndims}`.
