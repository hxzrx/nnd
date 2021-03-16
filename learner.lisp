(in-package #:nnd)

(defparameter *epsilon* 0.00001)

(defclass learner ()
  ((name :initarg :name
         :accessor name
         :type string
         :documentation "The learner's name.")
   (weights :initarg :weights
            :accessor weights
            :type list ;(or list array)
            :initform nil
            :documentation "the weights matrix of the network")
   (bias :initarg :bias
         :accessor bias
         :type (or list array)
         :initform nil
         :documentation "the bias of the network")
   (transfer :initarg :transfer
             :accessor transfer
             :type function
             :documentation "the transfer function"))
   (:documentation "a neural network learner, one layer"))

(defgeneric neural-network-output (learner input)
  (:documentation "get the output responds to input"))

(defmethod neural-network-output ((learner learner)  (input list))
  "get the output responds to input"
  (funcall (transfer learner)
           (matrix-add (matrix-product (weights learner) input)
                       (bias learner))))

(defgeneric training (learner samples)
  (:documentation "training a neural network learner"))

(defmethod training ((learner learner) (samples list))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; multi-layer network, get the result of the network
(defgeneric cascaded-network (input weights biases transfers)
  (:documentation "Propagate the input forward through the network, used for multilayer network"))

(defmethod cascaded-network ((input list) (weights list) (biases list) (transfers list))
  "weights is the the list of weights for all the layers"
  ;; (cascaded-network '((-1) (1)) '( ((1 -1) (1 0)) ((1 1)) ) '( ((1) (2)) 1 ) (list #'cube #'purelin)) ;1
  (if (null weights)
      input
      (cascaded-network
       (funcall (car transfers)
                (matrix-add (matrix-product (car weights) input)
                            (car biases)))
       (cdr weights)
       (cdr biases)
       (cdr transfers))))

(defmethod cascaded-network ((input number) (weights list) (biases list) (transfers list))
  "weights is the the list of weights for all the layers"
  ;; (cascaded-network -1 '(-1 -2) '(1 1) (list #'tansig #'tansig)) ;-0.7296858
  ;; (cascaded-network 1 '( ((-0.27) (-0.41)) ((0.09 -0.17))  ) '( ((-0.48) (-0.13)) 0.48 ) (list #'logsig #'purelin)) ;0.446282
  (if (null weights)
      input
      (cascaded-network
       (funcall (car transfers)
                (matrix-add (matrix-product (car weights) input)
                            (car biases)))
       (cdr weights)
       (cdr biases)
       (cdr transfers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass static-network ()
  ((neurons :initarg :neurons
            :accessor neurons
            :type list
            :initform nil
            :documentation "the list of neurons for the layers, R-S¹-S²-...-Sᴹ, where R is the input dimension, and Sⁱ is the neurons of each layer, this slot's value is (list R S¹ S² ... Sᴹ)")
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
   (input-proc :initarg :input-proc :accessor input-proc :type list :initform nil
               :documentation "describes the process type of Wp or ||W-p||, the type is a list of keywords for each layer, the valid keywords are :*, :|dist| and :-|dist|, default :*")
   (bias-proc :initarg :bias-proc :accessor bias-proc :type list :initform nil
              :documentation "describe the process type of Wp + b or Wp .* p, the type is a list of keywords for each layer, the valid keywords are :+ and :.*, defalt :+")
   (transfers :initarg :transfers
              :accessor transfers
              :type list
              :initform nil
              :documentation "the list of transfer functions for the layers")
   (neuron-outputs :initarg :neuron-outputs :accessor neuron-outputs :type list :initform nil
                   :documentation "temporary storage the list of the output of each layer for one forward propagation for an input"))
  (:documentation "A static network with a list of weights , a list of biases, etc."))

(defun make-static-network (&key neurons weights biases input-proc bias-proc transfers)
  (make-instance 'static-network
                 :neurons neurons
                 :weights weights
                 :biases biases
                 :input-proc input-proc
                 :bias-proc bias-proc
                 :transfers transfers))

(defmethod initialize-instance :after ((network static-network) &key &allow-other-keys)
  (with-slots ((neurons neurons)
               (weights weights)
               (biases biases)
               (input-proc input-proc)
               (bias-proc bias-proc)
               (summers summers)
               (transfers transfers)) network
    (when (null neurons) ;initialize neurons from weights
      (setf neurons (neurons-from-weights weights)))
    (when (null biases)
      (if weights
          (setf biases (neurons-to-random-biases neurons 0 0))
          (setf biases (neurons-to-random-biases neurons -0.5 0.5))))
    (when (null weights) ;initialize with random matrices
      (setf weights (neurons-to-random-weights neurons -0.5 0.5)))
    (when (null input-proc)
      (setf input-proc (loop for n in (cdr neurons)
                             collect :*)))
    (when (null bias-proc)
      (setf bias-proc (loop for n in (cdr neurons)
                            collect :+)))
    (when (every #'symbolp transfers)
      (setf transfers (loop for sbl in transfers
                            collect (symbol-function sbl))))))

(defmethod format-string ((network static-network))
  (with-slots ((neurons neurons)
               (weights weights)
               (biases biases)
               (input-proc input-proc)
               (bias-proc bias-proc)
               (transfers transfers)) network
    (format nil "neurons: ~d~&weights:~&~{~d~^~&~}~&biases:~{~d~^~&~}~&input-proc: ~{~s~^ ~}~&bias-proc: ~{~s~^ ~}~&transfers:~{~d~^ ~}~&"
            neurons
            weights
            (loop for b in biases collect (transpose b))
            input-proc
            bias-proc
            transfers)))

(defmethod print-object ((network static-network) stream)
  (print-unreadable-object (network stream :type t)
    (format stream (format-string network))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-neurons ((network static-network))
  (neurons network))

(defmethod get-weights ((network static-network))
  (weights network))

(defmethod get-bias ((network static-network))
  (biases network))

(defmethod empty-neuron-outputs! ((network static-network))
  (with-slots ((neuron-outputs neuron-outputs)) network
    (setf neuron-outputs nil)))

(defmethod get-neuron-outputs ((network static-network) layer-id)
  (with-slots ((neuron-outputs neuron-outputs)) network
    (nth layer-id neuron-outputs)))

(defmethod add-neuron-outputs! ((network static-network) output)
  (with-slots ((neuron-outputs neuron-outputs)) network
    (setf neuron-outputs (append neuron-outputs (list output)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun weight-op-input (weight input op)
  "Wp or ||Wp|| or -||Wp||"
  (ecase op
    (:* (matrix-product weight input))
    (:|dist| (dist weight input))
    (:-|dist| (matrix-product -1 (dist weight input)))))

(defun Wp-op-bias (Wp bias op)
  "Wp+bias or Wp.*bias"
  (ecase op
    (:+ (matrix-add Wp bias))
    (:.* (element-wise-mul Wp bias))))

(defun cascaded-forward-output! (network weights biases input-proc bias-proc transfers input)
  (if weights
      (cascaded-forward-output! network
                                (cdr weights)
                                (cdr biases)
                                (cdr input-proc)
                                (cdr bias-proc)
                                (cdr transfers)
                                (let ((result
                                        (funcall (car transfers)
                                                 (Wp-op-bias (weight-op-input (car weights) input (car input-proc))
                                                             (car biases)
                                                             (car bias-proc)))))
                                  (add-neuron-outputs! network result)
                                  result))
      input))

(defmethod static-network-output! ((network static-network) input-vector)
  "calc the output of a static network"
  (with-slots ((weights weights)
               (biases biases)
               (input-proc input-proc)
               (bias-proc bias-proc)
               (transfers transfers)
               (neuron-outputs neuron-outputs)) network
    (setf neuron-outputs nil)
    (cascaded-forward-output! network weights biases input-proc bias-proc transfers input-vector)))

(defgeneric normalize-weight! (network nth-layer &optional normalized-len)
  (:documentation "normalize the weights matrix of the nth-layer of a static network so that all the rows have the same length")
  (:method ((network static-network) (nth-layer integer) &optional (normalized-len 1))
    (with-slots ((weights weights)) network
      (setf (nth nth-layer weights) (normalize-matrix (nth nth-layer weights ) normalized-len)))))
