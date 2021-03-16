(in-package #:nnd)

;;;; Chapter 16 (Chinese Edition), Radial Basis Networks





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; examples and exercises

(defun demo-page-330 ()
  (let ((network (make-static-network :neurons (list 1 2 1)
                                      :weights (list '((-1) (1)) '((1 1)))
                                      :biases (list '((2) (2)) 0)
                                      :input-proc (list :|dist| :*)
                                      :bias-proc (list :.* :+)
                                      :transfers (list #'radbas #'purelin)))
        (inputs '(-2 -1 0 1 2)))
    (format t "Initial network:~&~d~%~%" network)
    (loop for in in inputs
          do (format t "~%Input: ~d~&Output:~&~d~%" in (static-network-output! network in)))))

(defun demo-page-332 ()
  (let ((network (make-static-network :neurons (list 2 2 1)
                                      :weights (list '((-1 1) (1 -1)) '((2 2)))
                                      :biases (list '((1) (1)) -1)
                                      :input-proc (list :|dist| :*)
                                      :bias-proc (list :.* :+)
                                      :transfers (list #'radbas #'purelin)))
        (inputs '(((-1) (1)) ((1) (-1)) ((-1) (-1)) ((1) (1)))))
    (format t "Initial network:~&~d~%~%" network)
    (loop for in in inputs
          do (format t "~%Input: ~d~&Output:~&~d~%" in (static-network-output! network in)))))

(defun demo-page-336 ()
  "Chinese Edition, this example demonstrates the output of the 1st layer"
  (let ((network (make-static-network :neurons (list 1 3)
                                      :weights (list '((-2) (0) (2)))
                                      :biases (list '((0.5) (0.5) (0.5)))
                                      :input-proc (list :|dist|)
                                      :bias-proc (list :.*)
                                      :transfers (list #'radbas)))
        (inputs '(-2 -1.2 -0.4 0.4 1.2 2)))
    (format t "Initial network:~&~d~%~%" network)
    (loop for in in inputs
          do (format t "~%Input: ~d~&Output:~&~d~%" in (static-network-output! network in)))))
