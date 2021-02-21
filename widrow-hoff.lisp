(in-package #:nnd)

;;;; Chapter 10 Widrow-Hoff Learning
;;;; Apply the principles of performance learning to a single-layer linear neural network.
;;;; Widrow-Hoff learning is an approximate steepest descent algorithm, in which the performance index is mean square error.

(defgeneric adaline (input-vec weights-matrix bias-vec &key transfer)
  (:documentation
   "ADALINE(ADAptive LInear NEuron) network input-output.
    ADALINE network is very similar to the perceptron, except that its transfer function is linear, instead of hard-limiting."))

(defmethod adaline ((input-vec list) (weights list) (bias-vec list) &key (transfer #'purelin))
  "ADALINE network input-output, for list type of input and weights"
  (funcall transfer (matrix-add (matrix-product weights input-vec) bias-vec)))

(defmethod adaline ((input-vec list) (weights list) (bias number) &key (transfer #'purelin))
  "ADALINE network input-output, for list type of input and weights"
  ;;(format t "~&in adaline, input: ~d, weights: ~d, bias: ~d~%" input-vec weights bias)
  (funcall transfer (+ (matrix-product weights input-vec) bias)))

;;;; training with one sample
(defgeneric adaline-training-one-sample (sample weights alpha &key bias transfer)
  (:documentation "update weights and bias with one sample"))

(defmethod adaline-training-one-sample ((sample list) (weights list) (alpha number)
                                        &key (bias nil bias-supplied-p) (transfer #'purelin))
  "update weights and bias with one sample, sample has form (list input-vec label-vec), alpha is the learning rate"
  (let* ((sample-label (cadr sample))
         (label-size (when (listp sample-label) (matrix-size (transpose (list sample-label)))))
         (cur-bias (if (null bias)
                       (if (numberp sample-label) 0
                           (make-zeros (car label-size) 1))
                       bias))
         (a (adaline (transpose (list (car sample))) weights cur-bias :transfer transfer))
         (e (matrix-sub (if (numberp (cadr sample))  ; error = t - a  , note that matrix-sub can be applied to numbers and matrices
                            (cadr sample)
                            (transpose (list (cadr sample))))
                        a)))
    ;;(format t "a: ~d~&e: ~d~%" a e)
    ;;(format t "~&weight:~&~d~%" (matrix-add weights (matrix-product e (list (car sample)))))
    (if (zeros-p e) (cons weights bias)
        (cons (matrix-add weights (matrix-product (matrix-product e (list (car sample)))
                                                  (* 2 alpha)))
              (if bias-supplied-p
                  (matrix-add bias (matrix-product e (* 2 alpha)))
                  nil)))))

;;;; perception-training-one-turn%
(defgeneric adaline-training-one-turn% (samples weights alpha &key bias transfer)
  (:documentation "successive training with all the samples"))

(defmethod adaline-training-one-turn% ((samples list) (weights list) (alpha number)
                                          &key (bias nil bias-supplied-p) (transfer #'purelin))
  "return a cons of weights and bias"
  (if (null samples) (cons weights bias)
      (let ((updated-params (if bias-supplied-p
                                (adaline-training-one-sample (car samples) weights alpha :bias bias :transfer transfer)
                                (adaline-training-one-sample (car samples) weights alpha :transfer transfer))))
        (if bias-supplied-p
            (adaline-training-one-turn% (cdr samples) (car updated-params) alpha :bias (cdr updated-params) :transfer transfer)
            (adaline-training-one-turn% (cdr samples) (car updated-params) alpha :transfer transfer)))))


;;;; training one turn with all sample
(defgeneric adaline-training-one-turn (samples weights alpha &key bias transfer)
  (:documentation "training one turn with all sample"))

(defmethod adaline-training-one-turn ((samples list) (weights list) (alpha number)
                                         &key (bias nil bias-supplied-p) (transfer #'purelin))
  "training one turn with all samples, return a cons of weights and bias.
   for simplicity, input and labels in each sample were a list of numbers, and they will transpose to column vectors later.
   eg. '( ((1 2 3) (1 1))  ((1 1 1) (0 0)) )"
  (let* ((in-num  (length (car (car samples))))
         (out-num (if (integerp (cadr (car samples))) 1 (length (cadr (car samples)))))
         (init-weights (if weights weights
                      (rand-matrix out-num in-num 0 2))) ;elements between 0 and 2
         (init-bias (if (and bias-supplied-p  bias) ;; will treat as no bias when it is set nil
                        bias
                        (if (= out-num 1) (random 1.0) (rand-matrix out-num 1 0 1.0)))))
    ;(format t "~&in-num: ~d, out-num: ~d~&init-weights: ~d~&init-bias: ~d~%" in-num out-num init-weights init-bias)
    (if bias-supplied-p
        (adaline-training-one-turn% samples init-weights alpha :bias init-bias :transfer transfer)
        (adaline-training-one-turn% samples init-weights alpha :transfer transfer))))


(defgeneric sum-squares-errors (samples weights &key bias transfer)
  (:documentation "The sum of the squares of the errors will be examined as a measure of the quality of the ADALINE network."))

(defmethod sum-squares-errors ((samples list) (weights list) &key (bias nil) (transfer #'purelin))
  ""
  ;;(format t "~&sse, bias: ~d~%" bias)
  (let* ((label-sample (cadr (first samples)))
         (cur-bias (if (null bias)
                       (if (listp label-sample)
                           (make-zeros (car (matrix-size (transpose (list label-sample)))) 1)
                           0)
                       bias)))
    (loop for sample in samples
          sum
          (inner-product-self (matrix-sub (if (listp label-sample)
                                              (transpose (list (cadr sample)))
                                              (cadr sample))
                                          (adaline (transpose (list (car sample))) weights cur-bias :transfer transfer))))))


;;;; training with the whole examples
(defgeneric adaline-training (samples alpha &key init-weights init-bias threshold turns-limit transfer)
  (:documentation
   "training a adaline network with samples.
    samples has the form '((p1 . t1) (p2 . t2) ... (pn . tn),
    where p is a column vector and t is a number or a column vector, the number or vector element should only be 0 or 1"))


(defmethod adaline-training ((samples list) (alpha number)
                             &key (init-weights nil) (init-bias nil bias-supplied-p)
                               (threshold 0.001) (turns-limit 1000000) (transfer #'purelin))
  "training a adaline network with samples, return weights and bias"
  (assert (> alpha 0))
  (do* ((shuffled-samples samples (shuffle samples)) ;the 1st turn is not shuffled so as to verify with the book
        (weights init-weights (car training-result))
        (bias init-bias (cdr training-result))
        (training-result (if bias-supplied-p
                             (adaline-training-one-turn shuffled-samples weights alpha :bias bias :transfer transfer)
                             (adaline-training-one-turn shuffled-samples weights alpha            :transfer transfer))
                         (if bias-supplied-p
                             (adaline-training-one-turn shuffled-samples weights alpha :bias bias :transfer transfer)
                             (adaline-training-one-turn shuffled-samples weights alpha            :transfer transfer)))
        (delta-weights (matrix-sub (car training-result) weights)
                       (matrix-sub (car training-result) weights))
        (delta-sum-squares (sum-squares delta-weights)
                           (sum-squares delta-weights))
        (sse (sum-squares-errors shuffled-samples (car training-result) :bias (cdr training-result) :transfer transfer)
             (sum-squares-errors shuffled-samples (car training-result) :bias (cdr training-result) :transfer transfer))
        (turns 1 (incf turns)))
       ((or (< sse threshold)
            (>= turns turns-limit))
        (if (>= turns turns-limit)
            (progn (format t "~&Training Failed, after ~d turns, the weights did not converge, and the sum of squares of errors is still: ~f~%~%" turns sse))
            (progn (format t "~&Training Succefull!~%")
                   (format t "~&Turns: ~d, ~&Sum of squares of errors: ~f, ~&Delta-sum-squares: ~d~&Delta weights: ~%" turns sse delta-sum-squares)
                   (print-matrix delta-weights)
                   (print-training-result weights bias sse)
                   (list weights bias))))
    ;;(format t "~%~%Turn: ~d~&Delta sum squares: ~d~&Delta weights: ~%" turns delta-sum-squares)
    ;(print-matrix delta-weights)
    ;(print-training-result weights bias sse)
    ;(terpri)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; examles and exercises

(defun demo-adaline-one-sample ()
  "adaline-training-one-sample demo"
  (print (adaline-training-one-sample '((1 -1 1) (-1 -1)) '((1 1 -1) (-1 -1 1)) 0.01 :bias '((0) (0))))
  (terpri)
  (print (adaline-training-one-sample '((1 -1 1) (-1 -1)) '((1 1 -1) (-1 -1 1)) 0.01))
  (terpri)
  (print (adaline-training-one-sample '((1 -1 -1) -1) '((1 1 -1))  0.01 :bias 0))
  (terpri)
  (print (adaline-training-one-sample '((1 -1 -1) -1) '((1 1 -1))  0.01))
  (terpri))

(defun demo-adaline-training-one-turn ()
  "adaline-training-one-turn demo"
  (print (adaline-training-one-turn '(((1 -1 -1) -1) ((1 1 -1) 1)) nil 0.01))
  (terpri)
  (print (adaline-training-one-turn '(((1 -1 -1) -1) ((1 1 -1) 1)) nil 1/100 :bias 0))
  (terpri))


(defun example-page-158 ()
       "p158, Chinese Edition. Converged to (-3.1910115e-4 1.0002948 3.1910115e-4)"
       (adaline-training '(((1 -1 -1) -1) ((1 1 -1) 1)) 0.2 :init-weights '((0 0 0)) :turns-limit 100000 :threshold 0.000001))

(defun example-10.4 ()
  "page 168 P10.4, Chinese Edition. Converged to (0.0 1.0)"
  (adaline-training '(((1 1) 1) ((1 -1) -1)) 0.25 :init-weights '((0 0)) :turns-limit 1000000 :threshold 0.000001))

(defun example-10.8 ()
  "p171, Chinese Edition."
  (adaline-training '(((1 1) (-1 -1)) ((1 2) (-1 -1)) ((2 -1) (-1 1)) ((2 0) (-1 1)) ((-1 2) (1 -1)) ((-2 1) (1 -1)) ((-1 -1) (1 1)) ((-2 -2) (1 1))) 0.04 :init-weights '((-0.5948  -0.0523) (0.1667  -0.6667)) :init-bias '((0.0131) (0.1667)) :turns-limit 100000 :threshold 0.01))

(defun example-10.8+ ()
  "the first steps are correct, but the result is not, I have not found why."
  (adaline-training '(((1 1) (-1 -1)) ((1 2) (-1 -1)) ((2 -1) (-1 1)) ((2 0) (-1 1)) ((-1 2) (1 -1)) ((-2 1) (1 -1)) ((-1 -1) (1 1)) ((-2 -2) (1 1))) 0.04 :init-weights '((1 0) (0 1)) :init-bias '((1) (1)) :turns-limit 100000 :threshold 0.001))

(defun example-10.9 ()
  "page 172, Chinese edition"
  (adaline-training '(((1 -1 -1 -1 1 1 1 1 1 -1 -1 -1 -1 -1 -1 -1) 60)
                      ((1 1 1 1 1 -1 1 1 1 -1 1 1 -1 -1 -1 -1) 0)
                      ((1 1 1 1 1 1 -1 -1 1 -1 -1 -1 -1 -1 -1 -1) -60))
                    0.03
                    :init-weights '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                    :init-bias 0
                    :turns-limit 1000000 :threshold 0.000001))
