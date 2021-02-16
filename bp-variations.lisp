(in-package #:nnd)

;;;; Chapter 12, Variations on Backpropagation

(defun interval-location (f init-point direction epsilon)
  "evaluation F(x0 + ε*p0), F(x0 + 2ε*p0), F(x0 + 4ε*p0), ..., and stops when F has a successive incresement"
  (flet ((eval-fun (f init-point direction epsilon) ;evaluation function for a quadratic function, F(x0 + ε*p0)
           (funcall f (matrix-add init-point
                                  (matrix-product epsilon direction)))))
    (do* ((i 0 (incf i))
          (ep epsilon (* 2 ep))
          (F-prev (eval-fun f init-point direction 0) Fx)
          (Fx (eval-fun f init-point direction ep) (eval-fun f init-point direction ep)))
         ((> Fx F-prev) (cons (/ ep 4) ep)) ;need the interval of the last three ε's
      #+:ignore(format t "~&i: ~d, ε=~f, F_k-1=~f, F_k=~f~%" i ep F-prev Fx))))
#|
;;page 233, P12.4
(setf pfun1 (quadratic-function '((2 1) (1 2))))
(interval-location pfun1 '((0.8) (-0.25)) '((-1.35) (-0.3)) 0.075)
|#

(defun golden-section-search (f init-point direction a0 b0 &optional (tao 0.618) (tolerance 0.01))
  "used in interval reduction"
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
#|
;;page 233, P12.4
(setf pfun2 (quadratic-function '((2 1)(1 2))))
(golden-section-search pfun2 '((0.8) (-0.25)) '((-1.35) (-0.3)) 0.15 0.6)
|#

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

#|
;;page 231, P12.3
(setf f (quadratic-function '((2 0) (0 50))))
(setf g (list #'(lambda (x1 x2) (* 2 x1)) #'(lambda (x1 x2) (* 50 x2))))
(variable-learning-rate f g '((0.5) (0.5)) 0.05 0.2 1.5 0.5 0.05)
|#


(defclass lmbp-network (bp-network)
  ((jacobian :initarg :jacobian
             :accessor jacobian
             :type list
             :initform nil
             :documentation "jacobian matrix")
   (marquardt-sensitive :initarg :marquardt-sensitive
                        :accessor marquardt-sensitive
                        :type list
                        :initform nil
                        :documentation "the list of marquardt-sensitives of all the parameters")
   (neuron-nums :initarg :neuron-nums
                :accessor neuron-nums
                :type list
                :initform nil
                :documentation "the list of the neuron num of each layer")
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

(defun make-lmbp-network (&key weight-list bias-list transfer-list derivative-list)
  "return a bp-network instance with the initial parameters"
  (make-instance 'lmbp-network
                 :weights weight-list :biases bias-list :transfers transfer-list
                 :derivatives (loop for d-type in derivative-list
                                    collect (derivative d-type))))

(defmethod initialize-instance :after ((lmbp lmbp-network) &key &allow-other-keys)
  ""
  (flet ((layer-param-sum (a) (+ (car a) (1+ (cdr a)))))
    (with-accessors ((neurons neuron-nums)
                     (parameters parameter-num)) lmbp
      (setf neurons (loop for w in (weights lmbp) collect (if (numberp w) 1 (length w))))
      (setf parameters (loop for w in (weights lmbp) sum (if (numberp w) 2 (layer-param-sum (matrix-size w))))))))

(defun reset-squared-error-sum! (lmbp)
  "reset squared-error-sum of lmbp to zero"
  (setf (squared-error-sum lmbp) 0))

(defun update-squared-error-sum! (lmbp samples)
  "update squared-error-sum of lmbp"
  (reset-squared-error-sum! lmbp)
  (loop for sample in samples
        do (progn
             (let ((a (propagation-forward-without-states lmbp (if (numberp (car sample))
                                                                  (car sample)
                                                                  (list (car sample)))))
                   (target (if (numberp (cadr sample)) (cadr sample) (transpose (list (cadr sample))))))
               (incf (squared-error-sum lmbp) (inner-product-self (matrix-sub target a))))))
  (squared-error-sum lmbp))


#+:ignore
(setf lmbp1 (make-lmbp-network :weight-list '(1 2)
                               :bias-list '(0 1)
                               :transfer-list (list #'square #'purelin)
                               :derivative-list (list :square :purelin)))

(defun parameter-type-list (weights biases)
  "return a list of flatten type such as (:w :w :w :w :b :b) to denote an element of a row of Jacobian matrix corresponding to a weight or a bias"
  (when weights
    (append (append (if (numberp (car weights)) (list :w)
                        (make-list (* (car (matrix-size (car weights))) (cdr (matrix-size (car weights))))
                                   :initial-element :w))
                    (if (numberp (car biases)) (list :b)
                        (make-list (car (matrix-size (car biases)))
                                   :initial-element :b)))
            (parameter-type-list (cdr weights) (cdr biases)))))

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
  ;(format t "~&sens: ~d, weights: ~d, inputs: ~d, tpye: ~d~%" marquardt-sensitivity weights inputs type)
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

(defmethod calc-jacobian% ((lmbp lmbp-network) (sample list))
  "calc the rows of the jacobian matrix corresponding to one sample"

  (let* ((a (propagation-forward lmbp (if (numberp (car sample))
                                          (car sample)
                                          (list (car sample)))));propagation forward
         (weight-list (reverse (weights lmbp)))
         (bias-list (reverse (biases lmbp)))
         (input-list (inputs lmbp))
         (net-input-list (net-inputs lmbp))
         (dF (reverse (derivatives lmbp)))
         (target (if (numberp (cadr sample)) (cadr sample) (transpose (list (cadr sample)))))
         )
    (incf (squared-error-sum lmbp) (inner-product-self (matrix-sub target a)))
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
          ;;(format t "~&jacobian-block: ~d~%" jacobian-block)
          (setf (jacobian lmbp) (append (jacobian lmbp) jacobian-block))
          (jacobian lmbp))
      ;;(format t "~&jacobian-block: ~d, jacobian-matrix: ~d~%" jacobian-block jacobian-matrix)
      )))

#|
(setf lmbp2 (make-lmbp-network :weight-list '(1 2)
                               :bias-list '(0 1)
                               :transfer-list (list #'square #'purelin)
                               :derivative-list (list :square :purelin)))
(calc-jacobian% lmbp2 '(1 1))
;; should return '((-4 -4 -1 -1))
(calc-jacobian% lmbp2 '(2 2))
;; should return '((-4 -4 -1 -1) (-16 -8 -4 -1))
|#

(defmethod calc-jacobian ((lmbp lmbp-network) (samples list))
  "append jacobian matrix' blocks with respect to each sample, and get the final jacobian matrix,
the side effect is to write into the jacobian slot of `lmbp"
  (dolist (sample samples (jacobian lmbp))
    (calc-jacobian% lmbp sample))
  (jacobian lmbp))

#|
;; Solved problem, P12.5, page 234, Chinese Ed.
(setf lmbp2 (make-lmbp-network :weight-list '(1 2)
                               :bias-list '(0 1)
                               :transfer-list (list #'square #'purelin)
                               :derivative-list (list :square :purelin)))
(calc-jacobian lmbp2 '((1 1) (2 2)))
|#

#|
;; Exercise E12.14, page 240, Chinese Ed.
(setf lmbp3 (make-lmbp-network :weight-list '(((-0.27) (-0.41)) ((0.09 -0.17)))
                               :bias-list '(((-0.48) (-0.13)) 0.48)
                               :transfer-list (list #'logsig #'purelin)
                               :derivative-list (list :logsig :purelin)))
(calc-jacobian lmbp3 '((1) (0)))
|#

(defun hessian-approximation (hessian mu)
  "Hessian matrix may not be invertible, so modificate it to the approximate Hessian matrix: G = H + μI, where μ>0"
  (alexandria:if-let ((inv (matrix-inverse hessian)))
    inv
    (hessian-approximation (matrix-add hessian (matrix-product mu (eye (length hessian)))) mu)))

(defun levenberg-marquardt-backpropagation (lmbp samples mu theta &optional (tolerance 0.01))
  "levenberg marquardt backpropagation algorithm, page 224, Chinese ed."
  (let ((x (transpose (parameters-to-list lmbp)))
        (mu mu)
        (theta theta))
    (do* ((jacobian (calc-jacobian lmbp samples)
                    (calc-jacobian lmbp samples))
          (JᵀJ (matrix-product (transpose jacobian) jacobian)
               (matrix-product (transpose jacobian) jacobian))
          (gradient (reduce #'matrix-product (list 2 (transpose jacobian) x))
                    (reduce #'matrix-product (list 2 (transpose jacobian) x)))
          )
         ((<(norm gradient) tolerance)
          (format t "~&lmbp converged. Gradient norm: ~f~%" (norm gradient))
          lmbp)
      (do* ((hessian-approx-inv (hessian-approximation JᵀJ mu)
                                (hessian-approximation JᵀJ mu))
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
            (setf mu (* mu theta))
            (setf x new-x))
        (setf mu (* mu theta))
        (setf (weights lmbp) old-weights)
        (setf (biases lmbp) old-biases))
      )))
