# diff-boundary-corrfn
[![CI tests](https://github.com/fatimp/diff-boundary-corrfn/actions/workflows/test.yml/badge.svg)](https://github.com/fatimp/diff-boundary-corrfn/actions/workflows/test.yml)

This library provides a function which calculates the surface-surface
correlation function (see Salvatore Torquato's book "Random heterogeneous
materials") for a two-dimensional set with differentiable (almost everywhere)
boundary. A set is defined as points which satisfy an inequation `f(x, y) > T`.
The function `f` must be defined using `cl-forward-diff` package and have
`cl-forward-diff:differentiable-multivariate` type. Let's look at this example:

~~~~{.lisp}
(defpackage math-function
  (:use #:cl)
  (:local-nicknames (:diff :cl-forward-diff))
  #.(cl-forward-diff:shadowing-import-math)
  (:export #:f))
(in-package :math-function)

(declaim (ftype diff:differentiable-multivariate f))
(defun f (coord)
  (declare (optimize (speed 3)))
  (destructuring-bind (x y) coord
    (declare (type diff:dual x y))
    (max (abs x)
         (abs y))))
~~~~

An inequation `(> (FUNCALL F COORD) 0.2)` defines a set `ℝ \ [-0.2, 0.2]^2` with
a boundary which is differentiable almost everywhere. Now evaluation of

~~~~{.lisp}
(diff-boundary-corrfn:surface-surface
 #'math-function:f
 0.2 '(0.1 0.1))
~~~~

gives `2.0`. This can be understood as that two boundaries of a square which lie
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

Currently all points of intersection must lie in a square `[-1, 1]`.
