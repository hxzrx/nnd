;;;; tests/util-tests.lisp

;;usage: (nnd-tests::run-nnd-tests)

(in-package #:nnd-tests)

(deftest test-list-length-equal ()
  (is (nnd::list-length-equal nil nil))
  (is (nnd::list-length-equal (list 1 2 3) (list 6 7 8)))
  (is (nnd::list-length-equal '(a b c) '(aa bb cc)))
  (is (not (nnd::list-length-equal nil '(a))))
  (is (not (nnd::list-length-equal '(1 2) nil)))
  (is (not (nnd::list-length-equal '(a) '(1 2 3))))
  (is (not (nnd::list-length-equal '(1 2 3) '(a)))))
      
(deftest test-list-check-type ()
  (is (not (nnd::list-check-type nil 'real)))
  (is (not (nnd::list-check-type '(1 a 2) 'real)))
  (is (not (nnd::list-check-type '(a 1 2) 'real)))
  (is (not (nnd::list-check-type '(1 2 a) 'real)))
  (is (not (nnd::list-check-type '(a) 'real)))
  (is (nnd::list-check-type '(1) 'real))
  (is (nnd::list-check-type '(1 2 3) 'real)))

(deftest test-lists-length-equal ()
  (is (nnd::lists-length-equal '((1 2 3) (4 5 6))))
  (is (nnd::lists-length-equal '(nil nil)))
  (is (nnd::lists-length-equal '((1) (2) (3) (4))))
  (is (not (nnd::lists-length-equal '((1 2 3) (4 5)))))
  (is (not (nnd::lists-length-equal '((1 2) (4 5 6))))))

(deftest test-list-check-rectangle ()
  (is (nnd::list-check-rectangle '((1 2 3))))
  (is (nnd::list-check-rectangle '((1 2 3) (4 5 6))))
  (is (nnd::list-check-rectangle '((1) (2) (3))))
  (is (not (nnd::list-check-rectangle '((1 2) (4 5 6)))))
  (is (not (nnd::list-check-rectangle '((1 2 3) (4 5)))))
  (is (not (nnd::list-check-rectangle '((1 2 3) (4 5) (7 8 9))))))
  
(deftest test-quadratic-function ()
  (let ((qfun1 (nnd::quadratic-function '((10 -6) (-6 10)) '((4) (4))))
        (x1 '((-1) (-2.5))))
    (is (funcall qfun1 x1) 7.25)))
    
    

