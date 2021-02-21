;;;; Chapter 14, Generalization

(in-package :nnd)

(defun estimate-generalization-error (learner output-function test-set performance-function)
  "learner: a neural network instance such as bp-network,
output-function: a method of `learner such as propagation-forward-without-states, it will make an output when giving an input ,
test-set: a data set for estimating error,
performance-function: a function, such as equare-error-sum-f, compute the error, this function may be implemented by a partial function"
  (funcall performance-function
           (loop for (data target) in test-set
                 collect (list (funcall output-function learner (list-to-vector data)) ; get (output target) list
                               (list-to-vector target)
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
         (jacobian (calc-jacobian! lmbp samples))
         (error-vector (error-vector lmbp)) ;v(x) in equations (12.20) and (12.22) and others
         (JᵀJ (matrix-product (transpose jacobian) jacobian))
         ;;(gradient (reduce #'matrix-product (list 2 (transpose jacobian) x)))
         )
    (format t "~&lmbp-one-step~%")
    (do* ((hessian-approx-inv (hessian-approximation-inv JᵀJ μ)
                              (hessian-approximation-inv JᵀJ μ))
          (Δx (reduce #'matrix-product (list -1 hessian-approx-inv (transpose jacobian) error-vector)) ; equation (12.32)
              (reduce #'matrix-product (list -1 hessian-approx-inv (transpose jacobian) error-vector)))
          (new-x (matrix-add x Δx) (matrix-add x Δx)) ; equation (12.31)
          (old-weights (weights lmbp) (weights lmbp))
          (old-biases (biases lmbp) (biases lmbp))
          (old-sse (squared-error-sum lmbp) (squared-error-sum lmbp)) ; F(x), computed when the jacobian was computed
          (dummy (list-to-parameters! lmbp (car (transpose new-x))) ; update weights and biases, use it's side effect
                 (list-to-parameters! lmbp (car (transpose new-x)))) ; and the returned value is not used
          (new-sse (update-squared-error-sum! lmbp samples)
                   (update-squared-error-sum! lmbp samples)))
         ((< new-sse old-sse) ;step 4
          (setf μ (/ μ θ))
          (setf x new-x)
          hessian-approx-inv)
      (setf μ (* μ θ))
      (setf (weights lmbp) old-weights)
      (setf (biases lmbp) old-biases))
    ))

(defun calc-Eᴅ (network train-set)
  "SSE about the network's output data"
  (loop for (input target) in train-set
        sum (inner-product-self
             (matrix-sub (propagation-forward-without-states network (list-to-vector input))
                         (list-to-vector target)))))

(defun calc-Eᴡ (network)
  ""
  (inner-product-self (parameters-to-list network)))

(defun hessian-approximate (jacobian alpha beta)
  "page 250, H≈2βJ'J + αI"
  (let ((size (cdr (matrix-size jacobian))))
    (matrix-add (reduce #'matrix-product (list 2 beta (transpose jacobian) jacobian))
                (reduce #'matrix-product (list 2 alpha (eye size))))))

(defun bayesian-regularization (network train-set &optional (tolerance 0.01))
  "algorithm in page 250, Chinese ed. or page 483 in English pdf ed.
currently the network should be lmbp type
"
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
    (format t "~&Init, Ed=~,3f, Ew=~,3f, β=~,3f, α=~,3f, γ=~,3f, F(x)=βEᴅ+αEᴡ=~,3f~%" Ed-init Ew-init beta alpha gamma Fx)
    (levenberg-marquardt-backpropagation network train-set)
    (do* ((i 1 (1+ i))
          (dummy (levenberg-marquardt-backpropagation network train-set)
                 (levenberg-marquardt-backpropagation network train-set))
          (jacobian (jacobian network) (jacobian network))
          (hessian (hessian-approximate jacobian alpha beta)
                   (hessian-approximate jacobian alpha beta))
          (hessian-inverse (matrix-inverse hessian) (matrix-inverse hessian))
          (sse (calc-Eᴅ network train-set) (calc-Eᴅ network train-set)) ;Eᴅ term
          (weight-pernalty (calc-Eᴡ network) (calc-Eᴡ network)) ;Eᴡ term
          (performance-prev Fx performace-Fx)
          (performace-Fx (+ (* beta sse) (* alpha weight-pernalty))
                         (+ (* beta sse) (* alpha weight-pernalty))))
         ((< (abs (- performace-Fx performance-prev)) tolerance)
          (setf gamma (- parameter-num (* 2 alpha (matrix-trace hessian-inverse))))
          (setf alpha (/ gamma (* 2 weight-pernalty)))
          (setf beta (/ (- all-output-dimensions gamma) (* 2 sse)))
          (format t "~&Converged!~%F(x)=βEᴅ+αEᴡ=~f~%" performace-Fx)
          (format t "~&β=~,3f, α=~,3f, γ=~,3f~%" beta alpha gamma)
          network)
      (format t "~&do* id: ~d~%" i)
      (when (mod i 1) (format t "~&i, F(x)=βEᴅ+αEᴡ=~f~%" performace-Fx))
      (setf gamma (- parameter-num (* 2 alpha (matrix-trace hessian-inverse))))
      (setf alpha (/ gamma (* 2 weight-pernalty)))
      (setf beta (/ (- all-output-dimensions gamma) (* 2 sse))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; examples and exercises

(defun exercise-13.15 (&optional (alpha 0.1))
  "page 269, E13.15"
  (let ((lmbp (make-lmbp-network :neuron-list (list 1 30 1)
                             :transfer-list (list #'logsig #'purelin)
                               :derivative-list (list :logsig :purelin)))
        (train-function #'(lambda (network train-data) (backpropagation network train-data alpha)))
        (performance-function ;it has the same effect as #'estimate-generalization-error, see the prints
          #'(lambda (network validation-set)
              (funcall (equare-error-sum-f)  ; proto of the performance function
                       (loop for (input target) in validation-set
                             collect (list (propagation-forward-without-states
                                            network
                                            (transpose (list-to-vector input)))
                                           (transpose (list-to-vector target)))))))
        (train-set (data-generator-uniform-noise #'(lambda (x) (1+ (sin (* (/ pi 2) x)))) -2 2 10 -0.1 0.1 :type :random))
        (validation-set (data-generator-uniform-noise #'(lambda (x) (1+ (sin (* (/ pi 2) x)))) -2 2 5 0 0.01 :type :random))
        (test-set (data-generator-accurate #'(lambda (x) (1+ (sin (* (/ pi 2) x)))) -2 2 20 :type :random)))

    (early-stopping% lmbp train-function train-set validation-set performance-function 10)

    (loop for (input target) in (sort test-set #'< :key #'first) ;test-set
          do (format t "~&~f ~,3f ~,3f~%"
                     input
                     (propagation-forward-without-states lmbp (list-to-vector input))
                     target))
    (format t "~&generalization error: ~d~%"
            (estimate-generalization-error lmbp #'propagation-forward-without-states test-set (equare-error-sum-f)))
    (format t "~&performance index: ~,4f~%" (funcall performance-function lmbp test-set))
    ))

(defun exercise-13.16 ()
  "page 269, E13.16, Chinese edition"
  (let ((bp (make-lmbp-network :neuron-list (list 1 30 1)
                             :transfer-list (list #'logsig #'purelin)
                             :derivative-list (list :logsig :purelin)))
        (train-set (data-generator-uniform-noise #'(lambda (x) (1+ (sin (* (/ pi 2) x)))) -2 2 10 -0.1 0.1 :type :random))
        (test-set (data-generator-accurate #'(lambda (x) (1+ (sin (* (/ pi 2) x)))) -2 2 20 :type :random)))

    (format t "~&parameter num: ~d~%" (parameter-num bp))
    (bayesian-regularization bp train-set)

    (loop for (input target) in (sort test-set #'< :key #'first)
          do (format t "~&~f ~,3f ~,3f~%"
                     input
                     (propagation-forward-without-states bp (list-to-vector input))
                     target))
    (format t "~&generalization error: ~d~%"
            (estimate-generalization-error bp #'propagation-forward-without-states test-set (equare-error-sum-f)))
    ))
