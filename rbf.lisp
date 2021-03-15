(in-package #:nnd)

;;;; Chapter 16 (Chinese Edition), Radial Basis Networks





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; examples and exercises

(defun example-page-336 ()
  "Chinese Edition, this example demonstrates the output of the 1st layer"
  (let ((network (make-static-network :neurons (list 1 3)
                                      :weights (list '((-2) (0) (2)))
                                      :biases (list '((0.5) (0.5) (0.5)))
                                      :input-proc (list :||)
                                      :bias-proc (list :.*)
                                      :transfers (list #'radbas)))
        (inputs '(-2 -1.2 -0.4 0.4 1.2 2)))
    (format t "Initial network:~&~d~%~%" network)
    (loop for in in inputs
          do (format t "~%Input: ~d~&Output:~&~d~%" in (static-network-output! network in)))))
