(in-package #:nnd)

;;;; Chapter 11, Backpropagation

;;;; backpropagation network class definition
(defclass bp-network ()
  ((neurons :initarg :neurons
            :accessor neurons
            :type list
            :initform nil
            :documentation "the list of neurons for the layers, R-S¹-S²-...-Sᴹ, where R is the input dimension, and Sⁱ is the neurons of each layer, this slot's value is (list R S¹ S² ... Sᴹ)")
   (inputs :initarg :inputs
           :accessor inputs
           :type list
           :initform nil
           :documentation "the list of inputs for the layers, used for updating parameters, reverse order")
   (weights :initarg :weights
            :accessor weights
            :type list
            :initform nil
            :documentation "the list of weight matrices for the layers")
   (biases :initarg :biases
         :accessor biases
         :type list
         :initform nil
         :documentation "the list of biases for the layers")
   (net-inputs :initarg :net-inputs
               :accessor net-inputs
               :type list
               :initform nil
               :documentation "the list of net inputs for the layers, reverse order")
   (transfers :initarg :transfers
              :accessor transfers
              :type list
              :initform nil
              :documentation "the list of transfer functions for the layers")
   (derivatives :initarg :derivatives
                :accessor derivatives
                :type list
                :initform nil
                :documentation "the derivatives of the transfer functions for the layers")
   (outputs :initarg :outputs
            :accessor outputs
            :type list
            :initform nil
            :documentation "the list of outputs for the layers, reverse order, inverse order")
   (sensitivities :initarg :sensitivities
                  :accessor sensitivities
                  :type list
                  :initform nil
                  :documentation "the list of sensitivities for the layers,
                                  this need only two values in the iteration,
                                  but I keep the whole for the sake of checking the intermediate steps,
                                  used in batch bp")
   (gradients-sum :initarg :gradients-sum
                  :accessor gradients-sum
                  :type list
                  :initform nil
                  :documentation "∑(sensitive) * (input)ᵀ, sum over the examples for each layer,
                                  used in batch learning, used in batch bp"))
   (:documentation "a neural network learner, for multi-layers"))

(defun make-bp-network (&key neuron-list weight-list bias-list transfer-list derivative-list)
  "return a bp-network instance with the initial parameters"
  (make-instance 'bp-network
                 :init-neurons neuron-list
                 :init-weights weight-list
                 :init-biases bias-list
                 :transfers transfer-list
                 :derivatives (loop for d-type in derivative-list
                                    collect (derivative d-type))))

(defmethod initialize-instance :after ((bp bp-network) &key init-weights init-biases init-neurons &allow-other-keys)
  "init-weights and init-neurons should not be non-nil at the same time, and should not be nil at the same time"
  (with-accessors ((neurons neurons)
                   (weights weights)
                   (biases biases)) bp
    (setf neurons init-neurons)
    (setf weights init-weights)
    (setf biases  init-biases)
    (when (or (and init-neurons init-weights) (and (null init-neurons) (null init-weights)))
      (error "neurons and weights should not be initialized to nil at the same time"))
    (when (and neurons weights)
      (error "neurons and weights should not be initialized to non-nil at the same time"))
    (when (not (or (and init-biases init-weights) (and (null init-biases) (null init-weights))))
      (error "weights and biases should be initialized to both nil or both non-nil"))
    (cond (init-neurons ;randomly initialize weights and biases
           (setf weights (neurons-to-random-weights init-neurons -0.5 0.5)) ;-0.5 to 0.5 is suggested in exercise 11.25, page 209
           (setf biases  (neurons-to-random-biases  init-neurons -0.5 0.5)))
          (init-weights ;update neurons after the weights are initialized
           (setf neurons (neurons-from-weights init-weights)))
          (t (format t "~&Unkown condition when initialize bp-network.~%")
             (error "Unkown condition when initialize bp-network.")))))


;;;; get the result of the network given the input and parameters
(defgeneric propagation-forward-without-states (bp input)
  (:documentation "propagation forward for ONE sample, and only collect the final result"))

(defmethod propagation-forward-without-states ((bp bp-network) (input list))
  "the input is an column vector such as '((-1) (1)), and the result is a column vector or a number"
  (cascaded-network input (weights bp) (biases bp) (transfers bp)))

(defmethod propagation-forward-without-states ((bp bp-network) (input number))
  "the input is an column vector such as '((-1) (1)), and the result is a column vector or a number"
  (cascaded-network input (weights bp) (biases bp) (transfers bp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; get the resuls of the network, as well gathering all the intermediate states
(defgeneric propagation-forward% (bp input weights biases transfers)
  (:documentation "collect all the intermediate states and change the respect slots (inputs, net-inputs, outputs)
                   of the instance of bp"))

(defmethod propagation-forward% ((bp bp-network) (input list) (weights list) (biases list) (transfers list))
  "when input is a column vector"
  (if (null weights)
      input
      (let* ((n (matrix-add (matrix-product (car weights) input)
                            (car biases)))
             (a (funcall (car transfers) n)))
        (push input (inputs bp))
        (push n (net-inputs bp))
        (push a (outputs bp))
        (propagation-forward% bp a (cdr weights) (cdr biases) (cdr transfers)))))

(defmethod propagation-forward% ((bp bp-network) (input number) (weights list) (biases list) (transfers list))
  "when input is a number"
  (if (null weights)
      input
      (let* ((n (matrix-add (matrix-product (car weights) input)
                            (car biases)))
             (a (funcall (car transfers) n)))
        (push input (inputs bp))
        (push n (net-inputs bp))
        (push a (outputs bp))
        (propagation-forward% bp a (cdr weights) (cdr biases) (cdr transfers)))))

(defgeneric propagation-forward (bp input)
  (:documentation "propagation forward, return the result, and collect all the intermediate states"))

(defmethod propagation-forward ((bp bp-network) (input list))
  "the input is an column vector such as '((-1) (1))"
  (setf (inputs     bp) nil) ;initialized to nil before propagating
  (setf (net-inputs bp) nil)
  (setf (outputs    bp) nil)
  (propagation-forward% bp input (weights bp) (biases bp) (transfers bp)))

(defmethod propagation-forward ((bp bp-network) (input number))
  "the input is an column vector such as '((-1) (1))"
  (setf (inputs     bp) nil)
  (setf (net-inputs bp) nil)
  (setf (outputs    bp) nil)
  (propagation-forward% bp input (weights bp) (biases bp) (transfers bp)))


(defgeneric derivative-diag (fun vars)
  (:documentation "F'(n) = diag(f'(n1) f'(n2) ... f'(nm))"))

(defmethod derivative-diag ((fun function) (vars list))
  "Chinese Ed. p185, (11.34)"
  ;;(derivative-diag (derivative :logsig) '(1 2 3))
  (diag-from-list (mapcar fun vars)))

;;(defgeneric sensitivity (bp ∂F target a m)
;;  (:documentation "sensicivity s = ∂F / ∂n"))

(defmethod sensitivity-init (derivative net-inputs  target a)
  "for the last layer, sᴹ = -2 ∂Fᴹ(nᴹ) (t - a)"
  (let ((F^M (if (numberp net-inputs)
                 (funcall derivative net-inputs)
                 (diag-from-list (loop for n in net-inputs collect (funcall derivative n)))))
        (e (matrix-sub target a)))
    (reduce #'matrix-product (list -2 F^m e))))

(defmethod sensitivity-update (derivative net-inputs weight+1 sensitivity+1)
  "sᵐ = ∂Fᵐ(nᵐ) (Wᵐ⁺¹)ᵀ sᵐ⁺¹"
  (let ((F^m (if (numberp net-inputs)
                 (funcall derivative net-inputs)
                 (diag-from-list (loop for n in net-inputs
                                       collect (funcall derivative (car n))))))) ;net-inputs is a column vec, so, use (car n)
    (reduce #'matrix-product (list F^m (transpose weight+1) sensitivity+1))))

(defgeneric backpropagation% (bp sample alpha)
  (:documentation "back propagation for one sample"))

(defmethod backpropagation% ((bp bp-network) (sample list) (alpha real))
  "back propagation for one sample"
  (let* ((a (propagation-forward bp (list-to-vector (first sample))));propagation forward and collect the intermediate states
         (new-weights nil)
         (new-bias nil)
         (weight-list (reverse (weights bp)))
         (bias-list (reverse (biases bp)))
         (input-list (inputs bp))
         (net-input-list (net-inputs bp))
         (dF (reverse (derivatives bp)))
         (target (if (numberp (cadr sample)) (cadr sample) (transpose (list (cadr sample)))))
         )
    (do* ((idx (1- (length (weights bp))) (decf idx))
          (next-weight nil (car cur-weights))
          (cur-weights weight-list (cdr cur-weights))
          (cur-bias (pop bias-list) (pop bias-list))
          (input (pop input-list) (pop input-list))
          (net-input (pop net-input-list) (pop net-input-list))
          (derivative (pop dF) (pop dF))
          (sensitivity (sensitivity-init derivative net-input target a)
                       (when derivative (sensitivity-update derivative net-input next-weight sensitivity))))
         ((= idx 0)
          ;; as the body part of this do* has side effects, so we have to duplicate this piece of code that's original in the body, otherwise, the first lay' weight and bias will be lost
          (push (matrix-sub (car cur-weights) (reduce #'matrix-product
                                                      (list alpha
                                                            sensitivity
                                                            (if (numberp input) input (transpose input)))))
                new-weights)
          (push (matrix-sub cur-bias (matrix-product alpha sensitivity))
                new-bias)

          (setf (weights bp) new-weights)
          (setf (biases bp) new-bias)
          bp)
      (push (matrix-sub (car cur-weights) (reduce #'matrix-product
                                                  (list alpha
                                                        sensitivity
                                                        (if (numberp input) input (transpose input)))))
            new-weights)
      (push (matrix-sub cur-bias (matrix-product alpha sensitivity))
            new-bias)
      )))

(defgeneric backpropagation (bp samples alpha)
  (:documentation "backpropagation for all the samples, update the parameters for each example"))

(defmethod backpropagation ((bp bp-network) (samples list) (alpha real))
  "note that only one turn of recursion for all samples"
  (dolist (sample samples)
    (backpropagation% bp sample alpha))
  bp)

(defgeneric backpropagation-batch (bp samples alpha)
  (:documentation "The total gradient of the mean square error is the mean of the gradients of the individual squared errors.
Therefore, to implement a batch version of the backpropagation algorithm,
we would executive propagate forward and calculate the sensitivities for all of the inputs in the training set.
Then, the individual gradients would be averaged to get the total gradient."))

(defmethod backpropagation-batch ((bp bp-network) (samples list) (alpha real))
  "we keep sum of the gradients in the slot of gradients-sum of bp and increase the value for each sample"
  ;; this function have bugs
  (let ((sample-num (length samples)))
  (do ((sample (pop samples) (pop samples))
       (sample-id 0 (incf sample-id)))
      ((null samples)
       (setf (weights bp)
             (loop for weight in (weights bp)
                   for gradient in (gradients-sum bp)
                   collect (matrix-sub weight (matrix-product (/ alpha sample-num)
                                                              gradient))))
       (setf (biases bp)
             (loop for bias in (biases bp)
                   for sensitivity in (sensitivities bp)
                   collect (matrix-sub bias (matrix-product (/ alpha sample-num)
                                                            sensitivity))))
       ;;(format t "~&Batch backpropagation completed~%")
       bp)
    ;;(format t "~&Sample id: ~d, contents: ~d~%" sample-id sample)
    (let ((a (propagation-forward bp (if (numberp (car sample))
                                         (car sample)
                                         (list (car sample)))));propagation forward and collect the intermediate states
          (weight-list (reverse (weights bp)))
          (bias-list (reverse (biases bp)))
          (input-list (inputs bp))
          (net-input-list (net-inputs bp))
          (dF (reverse (derivatives bp)))
          (new-gradients nil)
          (gradients (reverse (gradients-sum bp)))
          (new-sensitivities nil)
          (sensitivities (reverse (sensitivities bp)))
          (target (if (numberp (cadr sample)) (cadr sample) (transpose (list (cadr sample))))))
      (do* ((idx (length (weights bp)) (decf idx)) ;may have bug, if found, see backpropagation%
            (next-weight nil (car cur-weights))
            (cur-weights weight-list (cdr cur-weights))
            (cur-bias (pop bias-list) (pop bias-list))
            (input (pop input-list) (pop input-list))
            (net-input (pop net-input-list) (pop net-input-list))
            (g (pop gradients) (pop gradients))
            (s (pop sensitivities) (pop sensitivities))
            (derivative (pop dF) (pop dF))
            (sensitivity (sensitivity-init derivative net-input target a)
                         (when derivative (sensitivity-update derivative net-input next-weight sensitivity))))
           ((= idx 0)
            (setf (gradients-sum bp) new-gradients)
            (setf (sensitivities bp) new-sensitivities))
            ;;(format t "~&Accumulate gradient and sensitivity `~d` completed!~%" sample))
        (push (if g
                  (matrix-add g (matrix-product sensitivity (transpose input)))
                  (matrix-product sensitivity (transpose input)))
              new-gradients)
        (push (if s (matrix-add s sensitivity) sensitivity)
              new-sensitivities)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; examples and exercises
(defun exercise-11.12 ()
  "page 206, Chinese ed."
  (let ((bp (make-bp-network :weight-list '(((1 -1) (1 0)) ((1 1)))
                             :bias-list '(((1) (2)) 1)
                             :transfer-list (list #'cube #'purelin)))
        (p '((-1) (1))))
    (propagation-forward-without-states bp p)
    (propagation-forward bp p)
    (backpropagation% bp p 0.5)
    #+:ignore(backpropagation% bp '(-1 1) 0.5) ;will not work
    ))

(defun example-11.7 ()
  "page 198, Chinese ed."
  (let ((bp (make-bp-network :weight-list '(-1 -2)
                             :bias-list '(1 1)
                             :transfer-list (list #'tansig #'tansig))))
    (propagation-forward-without-states bp -1)
    (propagation-forward bp -1)))

(defun example-11.2.3 ()
  "page 186, Chinese ed."
  (let ((bp (make-bp-network :weight-list '(((-0.27) (-0.41)) ((0.09 -0.17)))
                             :bias-list '(((-0.48) (-0.13)) 0.48)
                             :transfer-list (list #'logsig #'purelin)))
        (p 1))
    (propagation-forward-without-states bp p)
    (propagation-forward bp p)
    (format t "~&New weights: ~d~%" (weights bp))
    (format t "~&New biases: ~d~%" (biases bp))))

(defun example-11.2.3+ ()
  "page 186, Chinese ed."
  (let ((bp (make-bp-network :weight-list '(((-0.27) (-0.41)) ((0.09 -0.17)))
                             :bias-list '(((-0.48) (-0.13)) 0.48)
                             :transfer-list (list #'logsig #'purelin)
                             :derivative-list (list :logsig :purelin)))
        (data (list (list 1 (1+ (sin (* (/ pi 4) 1))))
                    (list -2 (1+ (sin (* (/ pi 4) -2))))
                    (list 2 (1+ (sin (* (/ pi 4) 2)))))))
    (backpropagation% bp (list 1 (1+ (sin (/ pi 4)))) 0.1)
    (backpropagation  bp data 0.1)))

(defun example-11.7 ()
  "page 198, Chinese ed."
  (let ((bp (make-bp-network :weight-list '(-1 -2)
                             :bias-list '(1 1)
                             :transfer-list (list #'tansig #'tansig)
                             :derivative-list (list :tansig :tansig)))
        (data (list -1 1))
        (alpha 1))
    (backpropagation% bp data alpha)))

(defun exercise-11.25 ()
  "page 209, E11.25"
  (let ((bp (make-bp-network :neuron-list (list 1 10 1)
                             :transfer-list (list #'logsig #'purelin)
                             :derivative-list (list :logsig :purelin)))
        (data (data-generator-accurate #'(lambda (x) (1+ (sin (* (/ pi 2) x)))) -2 2 11)))
    (dotimes (i 10000) (backpropagation-batch bp data 0.1)) ;have unknown bugs
    ;;(dotimes (i 1000) (backpropagation bp data 0.2)) ;result correct
    (loop for (input target) in data
          do (format t "~&~f ~,3f ~,3f~%" input (propagation-forward-without-states bp input) target))
    ))

;; batch bp's example is in page 228
