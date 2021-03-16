(in-package #:nnd)

;;;; Chapter 16 (Chinese Edition), Radial Basis Networks

(defun linear-least-squares (input-vecs targets &optional (rho 0))
  "this assumes that a target is a scalar, so targets is a list of numbers"
  (let* ((input-rank (length (first input-vecs)))
         (input-num (length input-vecs))
         (reg-matrix (make-augmented (loop for input in input-vecs ;regression matrix
                                           append (transpose input))
                                     (make-ones input-num 1)))
         (reg-matrixᵀ (transpose reg-matrix))
         (target-vector (transpose (list targets)))
         (UᵀU (matrix-product reg-matrixᵀ reg-matrix))
         (rho-eye (matrix-product rho (eye (1+ input-rank))))
         (inv (matrix-inverse (matrix-add UᵀU rho-eye))))
    (reduce #'matrix-product (list inv reg-matrixᵀ target-vector))))







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
  "Chinese Edition, this example demonstrates the linear least squares utilized to set the parameters in layer2.
note that there are differences between the results of this demo with those in the textbook, and they are due to the instability of the inverse of the matrix"
  (let* ((network (make-static-network :neurons (list 1 3)
                                       :weights (list '((-2) (0) (2)))
                                       :biases (list '((0.5) (0.5) (0.5)))
                                       :input-proc (list :|dist|)
                                       :bias-proc (list :.*)
                                       :transfers (list #'radbas)))
         (inputs '(-2 -1.2 -0.4 0.4 1.2 2))
         (targets '(0 0.19 0.69 1.3 1.8 2))
         (layer-id 0)
         (neuron-outputs (loop for in in inputs
                               collect (let ((output (static-network-output! network in)))
                                         (declare (ignore output))
                                         (get-neuron-outputs network layer-id)))))
    (format t "Initial network:~&~d~%~%" network)
    (linear-least-squares neuron-outputs targets)))
