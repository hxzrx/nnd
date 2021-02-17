;;;; Chapter 14, Generalization

(in-package :nnd)

(defun estimate-generalization-error (learner output-function test-set performance-function)
  "learner: a neural network instance such as bp-network,
output-function: a method of `learner, it will make an output when giving an input,
test-set: a data set for estimating error,
performance-function: a function to compute the error, this function may be implemented by a partial function"
  (funcall performance-function
           (loop for (data target) in test-set
                 collect (list target  ; get (target  output) list
                               (funcall output-function learner data)))))

(defun equare-error-sum-f ()
  "performance function: Σ(t-a)ᵀ(t-a)"
  #'(lambda (target-output-cons)
      (loop for (target output) in target-output-cons
            sum (matrix-product (transpose (matrix-sub target output))
                                (matrix-sub target output)))))

(defun equare-error-sum-penalty-f (net-parameters beta alpha)
  "performance function: βΣ(t-a)ᵀ(t-a) + αΣx²,
net-parameters is a column vector of the net's parameters(weights and biases)"
  #'(lambda (target-output-cons)
      (+ (* beta (loop for (target output) in target-output-cons
                       sum (matrix-product (transpose (matrix-sub target output))
                                           (matrix-sub target output))))
         (* alpha (matrix-product (transpose net-parameters)
                                  net-parameters)))))


;;;; early stopping method for improving generalization
(defun early-stopping% (network training-f train-set validation-set performance-f &optional (error-inc-threshold 3))
  "If training is stopped before the minimum error sum is reached, then the network will be less likely to overfit.
Use cross-validation to decide when to stop."
  (let ((weights-queue (make-fixed-len-unsafe-fifo error-inc-threshold :content nil))
        (biases-queue (make-fixed-len-unsafe-fifo error-inc-threshold :content nil))
        (performance-value-queue (make-fixed-len-unsafe-fifo error-inc-threshold :content most-positive-fixnum)))
    (do* ((i 1 (incf i)))
         ((strict-ascending-list-p (get-contents performance-value-queue))
          (setf (weights network) (popq weights-queue))
          (setf (biases network) (popq biases-queue))
          (format t "~&Trained ~d turns.~%" i)
          network)
      (funcall training-f network train-set)
      (addq (weights network) weights-queue)
      (addq (biases network) biases-queue)
      (addq performance-value-queue (funcall performance-f network validation-set)))))

(defun early-stooping (network training-f data-set performance-f)
  "shuffle and then partition the data set, use cross validation to train the network"
  (let* ((part-ratio (list 70 15 15))
         (error-inc-threshold 3)
         (data (shuffle data-set))
         (data-partition (data-partition data part-ratio))
         (train-set (first data-partition))
         (validation-set (second data-partition))
         (test-set (third data-partition)))
    (early-stopping% network training-f train-set validation-set performance-f error-inc-threshold)
    (format t "~&Training stopped, error sum = ~f~%"
            (funcall performance-f network test-set))
    network))


;;;; Bayesian regularization method for improving generalization
