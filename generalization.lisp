;;;; Chapter 14, Generalization

(in-package :nnd)

(defun estimate-generalization-error (learner output-function test-set performance-function)
  "learner: a neural network instance such as bp-network,
output-function: a method of `learner such as propagation-forward-without-states, it will make an output when giving an input ,
test-set: a data set for estimating error,
performance-function: a function to compute the error, this function may be implemented by a partial function"
  (funcall performance-function
           (loop for (data target) in test-set
                 collect (list (funcall output-function learner data) ; get (target  output) list
                               target
                               ))))

(defun equare-error-sum-f ()
  "performance function: Σ(t-a)ᵀ(t-a), this is the proto function of performance"
  #'(lambda (output-target-list)
      (loop for (output target) in output-target-list
            sum (matrix-product (transpose (matrix-sub target output))
                                (matrix-sub target output)))))

(defun equare-error-sum-penalty-f (net-parameters beta alpha)
  "performance function: βΣ(t-a)ᵀ(t-a) + αΣx², this is the proto function of performance
net-parameters is a column vector of the net's parameters(weights and biases)"
  #'(lambda (output-target-list)
      (+ (* beta (loop for (output target) in output-target-list
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
          (format t "~&Trained ~d turns, performance index in validation set: ~,4f~%" i (popq performance-value-queue))
          network)
      (funcall training-f network train-set)
      (addq weights-queue (weights network))
      (addq biases-queue (biases network))
      (addq performance-value-queue (funcall performance-f network validation-set)))))

(defun early-stopping (network training-f data-set performance-f)
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
;;;; performance index: F(x) = βΣ(t-a)ᵀ(t-a) + αΣx² = βEᴅ + αEᴡ
(defun weights-square-sum (parameter-list)
  "the term Eᴡ = Σx², is the sum squared weights(all weights and biases)"
  (loop for x in parameter-list
        sum (* x x)))

(defun beta-bayes-re (variance)
  "β = 1/(2σ²), where σ² is the variance of each element of error ε, t = g(p) + ε"
  (/ (* 2 variance)))

(defun alpha-bayes-re (variance)
  "α = 1/(2σ²), where σ² is the variance of each of the weights"
  (/ (* 2 variance)))

(defun ZD-beta (beta N)
  "Zᴅ(β) = (π/β)^(N/2), where β is returned by beta-bayes-re, and N = Q * Sᴹ, Q is the sample num, Sᴹ is the dimension of the net output"
  (expt (/ pi beta) (/ N 2)))

(defun ZW-alpha (alpha n)
  "Zᴡ(α) = (π/α)^(n/2), where α is returned by alpha-bayes-re, and `n is the number of weights and biases in the network"
  (expt (/ pi alpha) (/ n 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun lmbp-one-step (lmbp samples &optional (theta 5) (mu 0.01))
  "execute one step of lmbp algorithm to locate the minimum point, return the lmbp object itself.
this function was reduced from levenberg-marquardt-backpropagation, and only execute one step in the outer do* loop.
this function return the reverse of Hessian matrix, H⁻¹"
  (let* ((θ theta)
         (μ mu)
         (x (transpose (parameters-to-list lmbp)))
         (jacobian (calc-jacobian lmbp samples))
         (JᵀJ (matrix-product (transpose jacobian) jacobian))
         ;;(gradient (reduce #'matrix-product (list 2 (transpose jacobian) x)))
         )
    (do* ((hessian-approx-inv (hessian-approximation JᵀJ μ)
                              (hessian-approximation JᵀJ μ))
          (Δx (reduce #'matrix-product (list -1 hessian-approx-inv (transpose jacobian) x))
              (reduce #'matrix-product (list -1 hessian-approx-inv (transpose jacobian) x)))
          (new-x (matrix-add x Δx) (matrix-add x Δx))
          (old-weights (weights lmbp) (weights lmbp))
          (old-biases (biases lmbp) (biases lmbp))
          (dummy (list-to-parameters! lmbp new-x) (list-to-parameters! lmbp new-x))
          (old-squared-error-sum (squared-error-sum lmbp) (squared-error-sum lmbp))
          (new-squared-error-sum (update-squared-error-sum! lmbp samples)
                                 (update-squared-error-sum! lmbp samples))
          (new-gradient (reduce #'matrix-product (list 2 (transpose jacobian) x))
                        (reduce #'matrix-product (list 2 (transpose jacobian) x))))
         ((< new-squared-error-sum old-squared-error-sum)
          (setf μ (* μ θ))
          (setf x new-x)
          hessian-approx-inv)
      (setf μ (* μ θ))
      (setf (weights lmbp) old-weights)
      (setf (biases lmbp) old-biases))
    ))

(defun calc-Eᴅ (network train-set)
  ""
  (loop for (input target) in train-set
        sum (inner-product-self
             (matrix-sub (propagation-forward-without-states network (transpose (list input)))
                         (transpose (list target))))))

(defun calc-Eᴡ (network)
  ""
  (inner-product-self (parameters-to-list network)))

(defun bayesian-regularization (network train-set &optional (tolerance 0.01))
  "algorithm in page 250, Chinese ed. or page 483 in English pdf ed.
currently the network should be lmbp type"
  (let* ((Ed-init (calc-Eᴅ network train-set))
         (Ew-init (calc-Eᴡ network))
         (parameter-num (parameter-num network)) ; n in the raw algorithm, the total number of parameters in the network
         (all-output-dimensions (* (length train-set) ; N in the raw algorithm, N=Q*Sᴹ
                                  (if (listp (second train-set))
                                      (length (second train-set))
                                      1))) ; should check the data format later
         (gamma parameter-num) ;gamma initialized to n
         (alpha (/ gamma (* 2 Ew-init)))
         (beta (/ (- all-output-dimensions gamma) (* 2 Ed-init)))
         (Fx (+ (* beta Ed-init) (* alpha Ew-init))))
    (format t "~&Init, F(x)=βEᴅ+αEᴡ=~f~%" Fx)
    (do* ((i 1 (1+ i))
          (hessian-inverse (lmbp-one-step network train-set) ;theta and mu use default value in lmbp-one-step
                           (lmbp-one-step network train-set))
          (square-error-sum (calc-Eᴅ network train-set) (calc-Eᴅ network train-set)) ;Eᴅ term
          (weight-pernalty (calc-Eᴡ network) (calc-Eᴡ network)) ;Eᴡ term
          (performance-prev Fx performace-Fx)
          (performace-Fx (+ (* beta square-error-sum) (* alpha weight-pernalty))
                         (+ (* beta square-error-sum) (* alpha weight-pernalty))))
         ((< (abs (- performace-Fx performance-prev)) tolerance)
          (format t "~&Converged!~% F(x)=βEᴅ+αEᴡ=~f~%" performace-Fx)
          network)
      (when (mod i 10) (format t "~&i, F(x)=βEᴅ+αEᴡ=~f~%" performace-Fx))
      (setf gamma (- parameter-num (* 2 alpha (matrix-trace hessian-inverse))))
      (setf alpha (/ gamma (* 2 weight-pernalty)))
      (setf beta (/ (- all-output-dimensions gamma) (* 2 square-error-sum))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; examples and exercises

(defun exercise-13.15 (&optional (alpha 0.1))
  "page 269, E13.15"
  (let ((lmbp (make-lmbp-network :neuron-list (list 1 30 1)
                             :transfer-list (list #'logsig #'purelin)
                               :derivative-list (list :logsig :purelin)))
        (train-function #'(lambda (network train-data) (backpropagation network train-data alpha)))
        (performance-function
          #'(lambda (network validation-set)
              (funcall (equare-error-sum-f)  ; proto of the performance function
                       (loop for (input target) in validation-set
                             collect (list (propagation-forward-without-states
                                            network
                                            (transpose (list-to-vector input)))
                                           (transpose (list-to-vector target)))))))
        (train-set (data-generator-gauss-noise #'(lambda (x) (1+ (sin (* (/ pi 2) x)))) -2 2 10 0 0.01 :type :random))
        (validation-set (data-generator-gauss-noise #'(lambda (x) (1+ (sin (* (/ pi 2) x)))) -2 2 5 0 0.01 :type :random))
        (test-set (data-generator-accurate #'(lambda (x) (1+ (sin (* (/ pi 2) x)))) -2 2 20 :type :random)))

    (early-stopping% lmbp train-function train-set validation-set performance-function 10)

    (loop for (input target) in test-set
          do (format t "~&~f ~,3f ~,3f~%"
                     input
                     (propagation-forward-without-states lmbp (list-to-vector input))
                     target))
    (format t "~&performance index: ~,4f~%" (funcall performance-function lmbp test-set))
    ))
