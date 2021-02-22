;;;; tests/linear-algebra-tests.lisp

(in-package #:nnd-tests)

(deftest test-matrix-pseudoinverse ()
  (is (equal (nnd::matrix-pseudoinverse '((1 1) (-1 1) (1 -1) (-1 -1)))
             '((1/4 -1/4 1/4 -1/4) (1/4 1/4 -1/4 -1/4)))))

(deftest test-kroncker-product ()
  (is (equal (nnd::kroncker-product '((1 2) (3 4)) '((5 6 7) (8 9 10)))
             '((5 6 7 10 12 14) (8 9 10 16 18 20) (15 18 21 20 24 28) (24 27 30 32 36 40)))))
