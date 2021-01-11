;;;; ./packages.lisp

(defpackage #:nnd
  (:use #:cl)
  (:export #:vector-add
           #:vector-sub
           #:vector-multiply-scalar
           #:matrix-add
           #:matrx-sub
           #:matrix-multiple-scalar
           #:matrix-divide-scalar
           #:rank-one-matrix-product
           #:matrix-product
           #:inner-product))
