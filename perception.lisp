(in-package #:nnd)

;;;; Chapter 4

(defgeneric perceptron (input-vec weights-matrix bias-vec &optional transfer-function)
  (:documentation "perception input-output"))

(defmethod perceptron ((input-vec list) (weights list) (bias-vec list) &optional (transfer-function #'hardlim))
  "perception input-output, for list type of input and weights"
  (funcall transfer-function (matrix-add (matrix-product weights input-vec) bias-vec)))

;;;; training with one sample
(defgeneric perception-training-one-sample (sample weights bias &optional transfer-function)
  (:documentation "update weights and bias with one sample"))

(defmethod perception-training-one-sample ((sample list) (weights list) (bias list) &optional (transfer-function #'hardlim))
  "update weights and bias with one sample, sample has form (list input-vec label-vec)"
  ;;test (nnd::perception-training-one-sample '((1 2) (0 1)) '((1 2) (2 1))  '((0) (1)) #'nnd::hardlim)
  ;;test (nnd::perception-training-one-sample '((1 2 1) (0 1)) '((1 2 1) (2 1 1))  '((0) (1)) #'nnd::hardlim)
  ;;(format t "~&sample: ~d~%" sample)
  ;;(format t "~&weight: ~d~%" weights)
  ;;(format t "~&bias  : ~d~%" bias)
  (let* ((a (funcall transfer-function (matrix-add (matrix-product weights (transpose (list (car sample)))) bias))) ; e = t - a
         (e (matrix-sub (transpose (list (cadr sample))) a)))
    ;;(format t "a: ~d~&e: ~d~%" a e)
    ;;(format t "~&weight:~&~d~%" (matrix-add weights (matrix-product e (list (car sample)))))
    (if (zeros-p e) (cons weights bias)
        (cons (matrix-add weights (matrix-product e (list (car sample))))
              (matrix-add bias e)))))

;;;; perception-training-one-turn%
(defgeneric perception-training-one-turn% (samples weights bias)
  (:documentation "successive training with all the samples"))

(defmethod perception-training-one-turn% ((samples list) (weights list) (bias list))
  (if (null samples) (cons weights bias)
      (let ((updated-params (perception-training-one-sample (car samples) weights bias)))
        (perception-training-one-turn% (cdr samples) (car updated-params) (cdr updated-params)))))

;;;; training one turn with all sample
(defgeneric perception-training-one-turn (samples &optional weights bias)
  (:documentation "training one turn with all sample"))

(defmethod perception-training-one-turn ((samples list) &optional weights bias)
  "training one turn with all samples. 
   for simplicity, input and labels in each sample were a list of numbers, and they will transpose to column vectors later.
   eg. '( ((1 2 3) (1 1))  ((1 1 1) (0 0)) )"
  ;;test (nnd::perception-training-one-turn '( ((1 1 3) (1 1))  ((1 1 1) (0 0)) ))
  (let* ((in-num  (length (car (car samples))))
         (out-num (if (integerp (cadr (car samples))) 1 (length (cadr (car samples)))))
         (init-weights (if weights weights
                      (rand-matrix out-num in-num 0 2))) ;elements between 0 and 2
         (init-bias (if bias bias
                        (rand-matrix out-num 1 0 2)))) ;1 column
    ;;(format t "~&in-num: ~d, out-num: ~d~&init-weights: ~d~&init-bias: ~d~%" in-num out-num init-weights init-bias)
    (perception-training-one-turn% samples init-weights init-bias)))


;;;; perceptron correctness
(defgeneric perceptron-correct-rate (samples weights bias &optional transfer-function)
  (:documentation "test with the samples"))

(defmethod perceptron-correct-rate ((samples list) (weights list) (bias list) &optional (transfer-function #'hardlim))
  ""
  (let ((correct-num 0)
        (sample-num  0))
    (loop for s in samples
          do (progn
               (incf sample-num)
               (when (equal (perceptron (transpose (list (car s))) weights bias transfer-function)
                            (transpose (list (cadr s))))
                 (incf correct-num))))
    (format t "~&Samples: ~d~&Correctly classified: ~d~&Correct rate: ~f" sample-num correct-num (/ correct-num sample-num))
    (/ correct-num sample-num)))
  
  
;;;; training one turn
(defgeneric perception-training (samples classified-rate)
  (:documentation "training a perception with samples. samples has the form '((p1 . t1) (p2 . t2) ... (pn . tn), where p is a column vector and t is a number or a column vector, the number or vector element should only be 0 or 1"))



          
