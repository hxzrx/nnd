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

