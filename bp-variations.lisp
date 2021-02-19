(in-package #:nnd)

;;;; Chapter 12, Variations on Backpropagation


(defclass lmbp-network (bp-network)
  ((jacobian :initarg :jacobian
             :accessor jacobian
             :type list
             :initform nil
             :documentation "jacobian matrix")
   (error-vector :initarg :error-vector
                 :accessor error-vector
                 :type list
                 :initform nil
                 :documentation "a Q*Sᴹ vector, appended by the errors of all the samples")
   (marquardt-sensitive :initarg :marquardt-sensitive
                        :accessor marquardt-sensitive
                        :type list
                        :initform nil
                        :documentation "the list of marquardt-sensitives of all the parameters")
   (parameter-num :initarg :parameter-num
                  :accessor parameter-num
                  :type integer
                  :initform 0
                  :documentation "sum of the number of the elements of weights and biased of all layers")
   (squared-error-sum :initarg :squared-error-sum
                      :accessor squared-error-sum
                      :type real
                      :initform 0
                      :documentation "performance index, sum of squared errors over all inputs")
   )
  (:documentation "Levenberg-Marquardt backpropagation algorithm"))

(defun make-lmbp-network (&key neuron-list weight-list bias-list transfer-list derivative-list)
  "return a bp-network instance with the initial parameters"
  (make-instance 'lmbp-network
                 :init-neurons neuron-list
                 :init-weights weight-list
                 :init-biases bias-list
                 :transfers transfer-list
                 :derivatives (loop for d-type in derivative-list
                                    collect (derivative d-type))))

(defmethod initialize-instance :after ((lmbp lmbp-network) &key &allow-other-keys)
  ""
  (flet ((layer-param-sum (a) (* (cdr a) (1+ (car a))))) ; add 1 to denote the bias num
    (with-accessors ((parameters parameter-num)) lmbp
      (setf parameters (loop for w in (weights lmbp) sum (if (numberp w) 2 (layer-param-sum (matrix-size w))))))))


(defun interval-location (f init-point direction epsilon)
  "evaluation F(x0 + ε*p0), F(x0 + 2ε*p0), F(x0 + 4ε*p0), ..., and stops when F has a successive incresement, page 219"
  (flet ((eval-fun (f init-point direction epsilon) ;evaluation function for a quadratic function, F(x0 + ε*p0)
           (funcall f (matrix-add init-point
                                  (matrix-product epsilon direction)))))
    (do* ((i 0 (incf i))
          (ep epsilon (* 2 ep))
          (F-prev (eval-fun f init-point direction 0) Fx)
          (Fx (eval-fun f init-point direction ep) (eval-fun f init-point direction ep)))
         ((> Fx F-prev) (cons (/ ep 4) ep)) ;need the interval of the last three ε's
      #+:ignore(format t "~&i: ~d, ε=~f, F_k-1=~f, F_k=~f~%" i ep F-prev Fx))))

(defun golden-section-search (f init-point direction a0 b0 &optional (tao 0.618) (tolerance 0.01))
  "used in interval reduction, page 220"
  (flet ((eval-fun (f init-point direction epsilon) ;evaluation function for a quadratic function, F(x0 + ε*p0)
           (funcall f (matrix-add init-point
                                  (matrix-product epsilon direction)))))
    (do* ((iter-num 0 (incf iter-num))
          (Fc<Fd? nil (< Fc Fd))
          (c-prev nil c)
          (d-prev nil d)
          (Fc-prev nil Fc)
          (Fd-prev nil Fd)
          (a a0 (if Fc<Fd? a c))
          (b b0 (if Fc<Fd? d b))
          (c (+ a (* (- 1 tao) (- b a)))
             (if Fc<Fd?
                 (+ a (* (- 1 tao) (- b a)))
                 d-prev))
          (d (- b (* (- 1 tao) (- b a)))
             (if Fc<Fd?
                 c-prev
                 (- b (* (- 1 tao) (- b a)))))
          (Fc (eval-fun f init-point direction c)
              (if Fc<Fd?
                  (eval-fun f init-point direction c)
                  Fd-prev))
          (Fd (eval-fun f init-point direction d)
              (if Fc<Fd?
                  Fc-prev
                  (eval-fun f init-point direction d))))
         ((when (< (- b a) tolerance)
            a))
      (format t "~&~d: Fc=~f, Fd= ~f, a=~f, b=~f, c=~f, d=~f~%" iter-num Fc Fd a b c d)
      )))

(defun variable-learning-rate (f gradient init-point alpha gamma eta rho zeta &optional (tolerance 0.01))
  "f is the performance index function,
gradient is the list of gradient (presented as functions) for each variable
alpha is the learning rate
gamma is the momentum
eta is some number greater than 1, and alpha will multiply eta when f decrease
rho is some number less than 1, and alpha will multiply rho when f's increase rate is greater than zeta
tolerance is the termination factor, |f(x) - f(x-1)| < tolerance
returns (cons new-point new-alpha)"
  (let* ((α alpha) (γ gamma) (η eta) (ρ rho) (ζ zeta)
         (x init-point)
         (Fx (funcall f x))
         (ᐁF (funcall #'gradient-at-point gradient init-point))
         (Δx (reduce #'matrix-product (list (- γ 1) α ᐁF))))
    (do* ((i 1 (incf i))
          (x-tmp (matrix-add x Δx) (matrix-add x Δx))
          (Fx-tmp (funcall f x-tmp) (funcall f x-tmp)))
         ((< (abs (- Fx Fx-tmp)) tolerance) (cons x α))
      (cond ((>= (/ (- Fx-tmp Fx) Fx) ζ)
             (setf α (* ρ α))
             (setf γ 0))
            ((< Fx-tmp Fx) ;accept
             (setf x x-tmp)
             (setf Fx (funcall f x))
             (setf ᐁF (funcall #'gradient-at-point gradient x))
             (setf α (* η α))
             (if (= γ 0) (setf γ gamma)))
            ((< (/ (- Fx-tmp Fx) Fx) ζ)
             (setf x x-tmp)
             (setf Fx (funcall f x))
             (setf ᐁF (funcall #'gradient-at-point gradient x))
             (when (= γ 0) (setf γ gamma)))
            (t (format t "~&Unknown condition: F(x)=~f, F(x-tmp)=~f~%" Fx Fx-tmp)))
      (setf Δx (matrix-sub (matrix-product γ Δx)
                           (reduce #'matrix-product (list (- 1 γ) α ᐁF)))) ;Δx_k = γ Δx_k-1 (1-γ) α ᐁF_k
      (format t "~&i: ~d, α=~f, γ=~f, x=~d, F(x)=~f~%" i α γ x Fx)
      )))

(defun reset-squared-error-sum! (lmbp)
  "reset squared-error-sum of lmbp to zero"
  (setf (squared-error-sum lmbp) 0))

(defun update-squared-error-sum! (lmbp samples)
  "update squared-error-sum of lmbp"
  (reset-squared-error-sum! lmbp)
  (with-slots ((sse squared-error-sum)) lmbp
    (loop for (input target) in samples
          do (progn
               (let ((a (propagation-forward-without-states lmbp (list-to-vector input)))
                     (tg (list-to-vector target)))
               (incf sse (inner-product-self (matrix-sub tg a)))))))
  (squared-error-sum lmbp))

(defun parameters-to-list (network)
  "collect the elements of weights and biases of all layers of a network instance and transfer them into a row vector,
note this return a row vector, not column vector"
  (matrix-flatten
   (loop for weight in (weights network)
         for bias in (biases network)
         collect (append (if (numberp weight) (list weight) (car (matrix-flatten weight)))
                         (if (numberp bias) (list bias) (car (matrix-flatten (transpose bias))))))))

(defun list-to-parameters! (network lst)
  "eg. (list-to-parameters! lmbp2 '(1 0 2 1))"
  (let ((template-weights (weights network))
        (template-biases (biases network))
        (new-weights nil)
        (new-biases nil))
    (loop for template-weight in template-weights
          for template-bias in template-biases
          do (progn
               (push (if (listp template-weight)
                         (loop for row in template-weight
                                     collect (loop for col in row
                                                   collect (pop lst)))
                         (pop lst))
                     new-weights)
               (push (if (listp template-bias)
                         (loop for row in template-bias
                               collect (loop for col in row
                                             collect (pop lst)))
                         (pop lst))
                     new-biases)))
    (setf new-weights (reverse new-weights))
    (setf new-biases (reverse new-biases))
    (setf (weights network) new-weights)
    (setf (biases network) new-biases)
    (list new-weights new-biases)))

(defmethod marquardt-sensitivity-init (derivative net-inputs)
  "(12.46), Marquardt sensitivity, for the last layer, Sᴹ = -1 ∂Fᴹ(nᴹ),
it's a matrix, not a column vector compared to bp's sensitivity"
  (let ((F^M (if (numberp net-inputs)
                 (funcall derivative net-inputs)
                 (diag-from-list (loop for n in net-inputs collect (funcall derivative n))))))
    (matrix-product -1 F^m)))

(defmethod marquardt-sensitivity-update (derivative net-inputs weight+1 sensitivity+1)
  "(12.47), sᵐ = ∂Fᵐ(nᵐ) (Wᵐ⁺¹)ᵀ sᵐ⁺¹"
  (let ((F^m (if (numberp net-inputs)
                 (funcall derivative net-inputs)
                 (diag-from-list (loop for n in net-inputs
                                       collect (funcall derivative (car n))))))) ;net-inputs is a column vec, so, use (car n)
    (reduce #'matrix-product (list F^m (transpose weight+1) sensitivity+1))))

(defun jacobian-block (marquardt-sensitivity weights inputs &key type)
  "First, make a matrix with the same size of weights or biases, the element of this matrix is corresponding to the Jacobian matrix's respected element.
Second, flat this matrix and get an row vector.
Third, append weights' Jacobian row vector and biases' Jacobian row vector and get an row vector.(This step is executed in other place.)
The steps above get a row corresponding to one final output's component of partial derivative, so loop over the sensitivity matrix, we will get the full jacobian matrix' block on this layer."
  (let* ((sensitivity (if (numberp marquardt-sensitivity) (list (list marquardt-sensitivity)) marquardt-sensitivity))
         (weight (if (numberp weights) (list (list weights)) weights))
         (input (if (numberp inputs) (list (list inputs)) inputs)))
    (cond ((eq type :weight)
           (matrix-flatten
            (loop for sens-col in (transpose sensitivity) ;a column of sensitivity matrix corresponds to a row of Jacobian
                  collect (car (matrix-flatten
                                (loop for w in weight
                                      for s in sens-col
                                      collect (car (transpose (matrix-product s input)))))))))
          ((eq type :bias) (matrix-flatten (transpose sensitivity)))
          (t (error "Unknown type in function jacoban-block")))))

(defmethod calc-jacobian!% ((lmbp lmbp-network) (sample list))
  "calc the rows of the jacobian matrix corresponding to one sample,
side effect: will modify error-vector slot of lmbp, and will modidify squared-error-sum slot of lmbp"
  (let* ((a (propagation-forward lmbp (list-to-vector (first sample)))) ;;propagation forward
         (weight-list (reverse (weights lmbp)))
         (bias-list (reverse (biases lmbp)))
         (input-list (inputs lmbp))
         (net-input-list (net-inputs lmbp))
         (dF (reverse (derivatives lmbp)))
         (target (list-to-vector (second sample)))
         )
    (with-slots ((err error-vector)
                 (sse squared-error-sum)) lmbp
      (let ((e (matrix-sub target a)))
        (if (listp e) ;update error-vector
            (setf err (append err e))
            (setf err (append err (list (list e)))))
        (incf sse (inner-product-self e))))

    (do* ((idx (1- (length (weights lmbp))) (decf idx))
          (next-weight nil (car cur-weights))
          (cur-weights weight-list (cdr cur-weights))
          (cur-bias (pop bias-list) (pop bias-list))
          (input (pop input-list) (pop input-list))
          (net-input (pop net-input-list) (pop net-input-list))
          (derivative (pop dF) (pop dF))
          (marquardt-sensitivity (marquardt-sensitivity-init derivative net-input)
                                 (marquardt-sensitivity-update derivative net-input next-weight marquardt-sensitivity))
          (jacobian-block-weight (jacobian-block marquardt-sensitivity (car cur-weights) input :type :weight)
                                 (jacobian-block marquardt-sensitivity (car cur-weights) input :type :weight))
          (jacobian-block-bias (jacobian-block marquardt-sensitivity (car cur-weights) input :type :bias)
                               (jacobian-block marquardt-sensitivity (car cur-weights) input :type :bias))
          (jacobian-block (make-augmented jacobian-block-weight jacobian-block-bias)
                          (make-augmented (make-augmented jacobian-block-weight jacobian-block-bias)
                                          jacobian-block))
          )
         ((= idx 0)
          (setf (jacobian lmbp) (append (jacobian lmbp) jacobian-block))
          (jacobian lmbp)))))

(defmethod calc-jacobian! ((lmbp lmbp-network) (samples list))
  "append jacobian matrix' blocks with respect to each sample, and get the final jacobian matrix,
the side effect is to write into the jacobian slot of `lmbp, and will modify error-vector slot of lmbp, and will modidify squared-error-sum slot of lmbp"
  ;;reset to nil and then update the value in calc-jacobian!%
  (setf (error-vector lmbp) nil)
  (setf (jacobian lmbp) nil)
  (reset-squared-error-sum! lmbp)

  (dolist (sample samples (jacobian lmbp))
    (calc-jacobian!% lmbp sample))
  (jacobian lmbp))

(defun hessian-approximation-inv (hessian mu)
  "Hessian matrix may not be invertible, so modificate it to the approximate Hessian matrix: G = H + μI, where μ>0.
`mu will be summed up on each recursion until the matrix G=H+k*μI is invertable"
  (alexandria:if-let ((inv (matrix-inverse hessian)))
    inv
    (hessian-approximation-inv (matrix-add hessian (matrix-product mu (eye (length hessian)))) mu)))

(defun levenberg-marquardt-backpropagation (lmbp samples &optional (mu 0.01) (theta 10) (tolerance 0.01))
  "levenberg marquardt backpropagation algorithm, page 224, Chinese ed. mu and theta's default value is suggested in page 222"
  (let ((θ theta)
        (x (transpose (parameters-to-list lmbp)))
        (μ mu))
    ;;the outer do* executes step 1 and 2 in the algorithm
    (do* ((j 1 (incf j))
          (jacobian (calc-jacobian! lmbp samples) ;J(x) in equation (12.23), this step also gets error vector and squared error sum
                    (calc-jacobian! lmbp samples))
          (error-vector (error-vector lmbp) ;v(x) in equations (12.20) and (12.22) and others
                        (error-vector lmbp))
          (JᵀJ (matrix-product (transpose jacobian) jacobian)
               (matrix-product (transpose jacobian) jacobian))
          (gradient (reduce #'matrix-product (list 2 (transpose jacobian) error-vector)) ;equation (12.22)
                    (reduce #'matrix-product (list 2 (transpose jacobian) error-vector)))
          )
         ((<(norm gradient) tolerance)
          (format t "~&LMBP Converged in <~d> iterations! Gradient norm: ~,5f, SSE: ~,5f~%" j (norm gradient) (squared-error-sum lmbp))
          lmbp)
      (format t "~&SSE: ~,4f, Gradient Norn: ~,4f~%" (squared-error-sum lmbp) (norm gradient))
      ;; the inner do* executes step 3 and 4 in the algorithm
      (do* ((i 0 (incf i))
            (hessian-approx-inv (hessian-approximation-inv JᵀJ μ)
                                (hessian-approximation-inv JᵀJ μ))
            (Δx (reduce #'matrix-product (list -1 hessian-approx-inv (transpose jacobian) error-vector)) ; equation (12.32)
                (reduce #'matrix-product (list -1 hessian-approx-inv (transpose jacobian) error-vector)))
            (new-x (matrix-add x Δx) (matrix-add x Δx)) ; equation (12.31)
            (old-weights (weights lmbp) (weights lmbp))
            (old-biases (biases lmbp) (biases lmbp))
            (old-squared-error-sum (squared-error-sum lmbp) (squared-error-sum lmbp)) ;F(x), computed when the jacobian was computed
            (dummy (list-to-parameters! lmbp (car (transpose new-x))) ;update weights and biases
                   (list-to-parameters! lmbp (car (transpose new-x))))
            (new-squared-error-sum (update-squared-error-sum! lmbp samples)
                                   (update-squared-error-sum! lmbp samples))
            )
           ((< new-squared-error-sum old-squared-error-sum) ;step 4
            (setf μ (/ μ θ))
            (setf x new-x))
        ;;(format t "~&Old SSE: ~,3f, new SSE: ~,3f~%" old-squared-error-sum new-squared-error-sum)
        (setf μ (* μ θ))
        (setf (weights lmbp) old-weights)
        (setf (biases lmbp) old-biases))
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; examples and exercises

(defun example-12.4 ()
  "page 233, P12.4"
  (let ((pfun (quadratic-function '((2 1) (1 2)))))
    (interval-location pfun '((0.8) (-0.25)) '((-1.35) (-0.3)) 0.075)))

(defun example-12.4+ ()
  "page 232, Chinese ed."
  (let ((pfun (quadratic-function '((2 1) (1 2)))))
    (golden-section-search pfun '((0.8) (-0.25)) '((-1.35) (-0.3)) 0.15 0.6)))

(defun example-12.3 ()
  "page 231, P12.3"
  (let ((f (quadratic-function '((2 0) (0 50))))
        (g (list #'(lambda (x1 x2) (declare (ignore x2)) (* 2 x1))
                 #'(lambda (x1 x2) (declare (ignore x1)) (* 50 x2)))))
    (variable-learning-rate f g '((0.5) (0.5)) 0.05 0.2 1.5 0.5 0.05)))

(defun example-12.5 ()
  "page 234, Chinese ed."
  (let ((lmbp (make-lmbp-network :weight-list '(1 2)
                                 :bias-list '(0 1)
                                 :transfer-list (list #'square #'purelin)
                                 :derivative-list (list :square :purelin)))
        (data '((1 1) (2 2))))
    (format t "~&Jacobian matrix for the firse input ~d: ~d~%" (first data) (calc-jacobian!% lmbp (first data)))
    (format t "~&Jacobian matrix after one more input ~d: ~d~%" (second data) (calc-jacobian!% lmbp (second data)))
    ))

(defun example-12.5+ ()
  "page 234, Chinese ed."
  (let* ((lmbp (make-lmbp-network :weight-list '(1 2)
                                  :bias-list '(0 1)
                                  :transfer-list (list #'square #'purelin)
                                  :derivative-list (list :square :purelin)))
         (data '((1 1) (2 2)))
         (res (calc-jacobian! lmbp data)))
    (format t "~&Train set: ~d~&Result: ~d~%" data res)
    ))

(defun exercise-12.14 ()
  "page 240, Chinese ed."
  (let* ((lmbp (make-lmbp-network :weight-list '(((-0.27) (-0.41)) ((0.09 -0.17)))
                                  :bias-list '(((-0.48) (-0.13)) 0.48)
                                  :transfer-list (list #'logsig #'purelin)
                                  :derivative-list (list :logsig :purelin)))
         (data (data-generator-accurate #'(lambda (x) (1+ (sin (* (/ pi 2) x)))) 1 0 2))
         (jacobian (calc-jacobian! lmbp data)))
    (format t "~&Jacobian matrix:~&~d~%" jacobian)))

(defun exercise-12.16 ()
  "page 240, Chinese ed. The net structure is the same as exercise 11.25"
  (let* ((sdbp (make-bp-network :neuron-list (list 1 10 1)
                                :transfer-list (list #'logsig #'purelin)
                                :derivative-list (list :logsig :purelin)))
         (lmbp (make-lmbp-network :neuron-list (list 1 10 1)
                                  :transfer-list (list #'logsig #'purelin)
                                  :derivative-list (list :logsig :purelin)))
         (data (data-generator-accurate #'(lambda (x) (1+ (sin (* (/ pi 4) x)))) -2 2 11)))
    (declare (ignore sdbp))
    ;;(dotimes (i 10000) (backpropagation-batch sdbp data 0.1)) ;have unknown bugs
    (levenberg-marquardt-backpropagation lmbp data) ;LMBP, result correct
    ;;(dotimes (i 1000) (backpropagation sdbp data 0.2)) ;SDBP, result correct
    ;;(loop for (input target) in data
    ;;      do (format t "~&SDBP Training:~f ~,3f ~,3f~%" input (propagation-forward-without-states sdbp input) target))
    (loop for (input target) in data
          do (format t "~&LMBP Training:~f ~,3f ~,3f~%" input (propagation-forward-without-states lmbp input) target))
    ))
