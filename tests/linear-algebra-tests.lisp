;;;; tests/linear-algebra-tests.lisp

(in-package #:nnd-tests)

(deftest test-matrix-pseudoinverse ()
  (is (equal (nnd::matrix-pseudoinverse '((1 1) (-1 1) (1 -1) (-1 -1)))
             '((1/4 -1/4 1/4 -1/4) (1/4 1/4 -1/4 -1/4)))))
