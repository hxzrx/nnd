(in-package #:nnd)

(defgeneric perceptron (input-vec weights-matrix bias-vec &optional transfer-function)
  (:documentation "perception input-output"))

(defmethod perceptron ((input-vec list) (weights list) (bias-vec list) &optional (transfer-function #'hardlim))
  "perception input-output, for list type of input and weights"
  (funcall transfer-function (matrix-add (matrix-product weights input-vec) bias-vec)))


;;(defgeneric perception-training (samples
