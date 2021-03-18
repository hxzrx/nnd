(in-package #:nnd)

;;;; Chapter 16 (Chinese Edition), Radial Basis Networks

(defun calc-bias (weights)
  "equation (16.9) $b_i^1 = Sqrt(S^1) / d_max$, it is still not clear what the d_max is"
  (if (listp weights)
      (/ (sqrt (length weights))
         (first (sort (loop for v1 in weights
                            for v2 in (cdr weights)
                            collect (distance (list v1) (list v2)))
                      #'>)))
      1))

(defun make-ols-regression-matrix (input-vectors)
  (let ((weights (loop for input in input-vectors
                       collect (if-typep-let (row (transpose input)) #'numberp
                                             (list row)
                                             (first row)))))
    (transpose (append (loop for input in input-vectors
                             append (transpose (radbas (dist weights input))))
                       (make-ones 1 (length input-vectors))))))

(defun linear-least-squares (input-vecs targets &optional (rho 0))
  "this assumes that a target is a scalar, so targets is a list of numbers"
  (let* ((input-rank (length (first input-vecs)))
         (input-num (length input-vecs))
         (reg-matrix (make-augmented (loop for input in input-vecs ;regression matrix U
                                           append (transpose input))
                                     (make-ones input-num 1)))
         (reg-matrixᵀ (transpose reg-matrix))
         (target-vector (transpose (list targets)))
         (UᵀU (matrix-product reg-matrixᵀ reg-matrix))
         (rho-eye (matrix-product rho (eye (1+ input-rank))))
         (inv (matrix-inverse (matrix-add UᵀU rho-eye))))
    (reduce #'matrix-product (list inv reg-matrixᵀ target-vector))))

(defun calc-initial-basis (reg-matrix target-vector)
  "the return result is the the element in basis function collector with the maximum contrbute, bf-collector is a list whose element is (list 0 basis-id basis-vector contribute), basis-vector is m vector in the algorithm, 0 denotes the 0th orthed vector"
  (let* ((inputs-num (length reg-matrix))
         (bf-collector nil))
    (loop for i from 0 to inputs-num
          for ux in (transpose reg-matrix)
          do (progn (let* ((uᵀ (list ux)) (u (transpose uᵀ))
                           (m u) (mᵀ uᵀ)
                           (mᵀm (matrix-product mᵀ m))
                           (h (/ (matrix-product mᵀ target-vector) mᵀm))
                           (tᵀt (matrix-product (transpose target-vector) target-vector))
                           (o (/ (* h h mᵀm) tᵀt)))
                      (push (list 0 i o u) bf-collector))))
    (setf bf-collector (reverse bf-collector))
    (first (sort bf-collector #'> :key #'third))))

(defun sum-contributes% (bf-collect)
  (loop for (nil nil o nil) in bf-collect
        sum o))

(defun calc-inner-product-to-orthed-vectors (reg-matrix bf-collector)
  "calc $r_{j,k}^{(i)}$ for k = (length bf-collector), here, k is fixed, i and j are indices.
bf-collector was initialized in calc-initial-basis"
  (let ((Q (length reg-matrix)))
    (let ((res
            (loop for i from 0 to Q
                  for ux in (transpose reg-matrix) ;(list ux) is uᵀ, so u is (transpose (list ux))
                  when (null (find i bf-collector :key #'second))
                    append (loop ;for j from 0 below k
                                 for bf in bf-collector
                                 collect
                                 (let ((j (first bf)))
                                   (list (list i j) (/ (matrix-product (list ux) (fourth bf))
                                                       (inner-product-self (fourth bf)))))))))
      res)))

(defun calc-candidate-orth-vectors (reg-matrix bf-collector r-collector)
  "calc $m_k^{(i)}$ for k = (length bf-collectr)"
  (let ((Q (length reg-matrix)))
    (let ((res (loop for i from 0 to Q
          for ux in (transpose reg-matrix) ;(list ux) is uᵀ, so u is (transpose (list ux))
          when (null (find i bf-collector :key #'second))
            collect
            (list i ; i maps to the ith basis
                  (matrix-sub (transpose (list ux)) ;u_i
                              (reduce #'matrix-add  ;Sigma
                                      (loop for bf in bf-collector
                                            collect
                                            (let ((j (first bf)))
                                              (matrix-product (fourth bf)
                                                              (second (find (list i j)
                                                                            r-collector
                                                                            :key #'first
                                                                            :test #'equal)))))))))))
      res)))

(defun calc-inner-product-to-target (candidate-orth-vecs target-vector)
    "calc $h_k^{(i)}$ for k = (length bf-collector), here, k is fixed, i is the index.
candidate-orth-vecs gets from calc-candidate-orth-vectors"
  (let ((res (loop for (i m) in candidate-orth-vecs
                   collect
                   (list i (/ (inner-product m target-vector)
                              (inner-product m m))))))
    res))

(defun calc-contributes (candidate-orth-vecs h-collector target-vector)
  "calc $o_k^{(i)}$ for k = (length bf-collector), here, k is fixed, i is the index.
candidate-orth-vecs gets from calc-candidate-orth-vectors,
h-collector getf from calc-inner-product-to-target"
  (let* ((tᵀt (inner-product-self target-vector))
         (res
           (loop for (i h) in h-collector
                 collect (list i
                               (/ (* h h (inner-product-self (second (find i candidate-orth-vecs :key #'first)))) tᵀt)))))
    res))

(defun calc-h-optimal (orthed-m target-vector)
  "$h_i^* = m_i^T / (m_i^T m_i)$"
  (/ (inner-product orthed-m target-vector)
     (inner-product target-vector target-vector)))

(defun calc-layer-parameters (bf-collector r-jk target-vector)
  "equation (16.57), $x_n = h_n$ and $x_k = Sigma_{j=k+1}^n r_{j,k}x_j$.
since $h_i^* = m_i^T / (m_i^T m_i)$, h can be induced from bf-collector.
Notice: there seems to be an erra in the textbook , Sigma_{j=k+1}^n r_{j,k}x_j should be Sigma_{j=k+1}^n r_{k,j}x_j
r-jk was calculated in orthogonal-least-squares"
  (let* ((parameter-num (1- (length bf-collector))) ;n
         (parameter-plist (list parameter-num
                                (calc-h-optimal (fourth (find parameter-num bf-collector :key #'first)) ;x
                                                target-vector))))
    (loop for k from (1- parameter-num) downto 0
          do (setf (getf parameter-plist k)
                   (- (calc-h-optimal (fourth (find k bf-collector :key #'first))
                                      target-vector)
                      (loop for j from (1+ k) to parameter-num
                            sum (* (second (find (list k j) r-jk :key #'first :test #'equal))
                                     (getf parameter-plist j))))))
    parameter-plist))


(defun orthogonal-least-squares (reg-matrix target-vector &optional (delta 0.05))
  "page 340, this assumes that a target is a scalar, so targets is a list of numbers.
reg-matrix is U in the algorithm, target-vector is t in the algorithm, t = Ux + e.
Note that delta should not be too small, else the sum of o's will exceed 1.
"
  (let* ((bf-collector (list (calc-initial-basis reg-matrix target-vector))) ; (list 0 i o u)
         (r-jk nil)
         (contribute-list (list (third (first bf-collector))))
         (layer-parameters nil))
    (loop for k from 1 to (length reg-matrix)
          do (progn (let* ((r (calc-inner-product-to-orthed-vectors reg-matrix bf-collector))
                           (m (calc-candidate-orth-vectors reg-matrix bf-collector r))
                           (h (calc-inner-product-to-target m target-vector))
                           (o (calc-contributes m h target-vector))
                           (o-sort (sort o #'> :key #'second))
                           (max-o (second (first o-sort)))
                           (max-o-index (first (first o-sort))) ;i_k
                           )
                      (push max-o contribute-list)
                      (loop for j from 0 to (- k 1)
                            do (push (list (list j k) (second (find (list max-o-index j) r
                                                             :key #'first
                                                             :test #'equal)))
                                     r-jk))
                      (nconc bf-collector (list (list k max-o-index max-o (second (find max-o-index m :key #'first))) ))
                      (when (<= (- 1 (reduce #'+ contribute-list)) delta)
                        (format t "~&Get stop criteria: ~f, at k = ~f, delta = ~f~&contribute: ~{~f~^ ~}~%" (- 1 (reduce #'+ contribute-list)) k delta contribute-list)
                        (return-from nil)))))
    (setf layer-parameters (calc-layer-parameters bf-collector r-jk target-vector))
    (format t "~&layer-parameters: ~d~%" layer-parameters)
    (list bf-collector layer-parameters)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; examples and exercises

(defun demo-page-330 ()
  "demonstrates rbd network's output"
  (let ((network (make-static-network :neurons (list 1 2 1)
                                      :weights (list '((-1) (1)) '((1 1)))
                                      :biases (list '((2) (2)) 0)
                                      :input-proc (list :|dist| :*)
                                      :bias-proc (list :.* :+)
                                      :transfers (list #'radbas #'purelin)))
        (inputs '(-2 -1 0 1 2)))
    (format t "Initial network:~&~d~%~%" network)
    (loop for in in inputs
          do (format t "~%Input: ~d~&Output:~&~d~%" in (static-network-output! network in)))))

(defun demo-page-332 ()
  "a pattern classifying example demonstrates rbd network's output"
  (let ((network (make-static-network :neurons (list 2 2 1)
                                      :weights (list '((-1 1) (1 -1)) '((2 2)))
                                      :biases (list '((1) (1)) -1)
                                      :input-proc (list :|dist| :*)
                                      :bias-proc (list :.* :+)
                                      :transfers (list #'radbas #'purelin)))
        (inputs '(((-1) (1)) ((1) (-1)) ((-1) (-1)) ((1) (1)))))
    (format t "Initial network:~&~d~%~%" network)
    (loop for in in inputs
          do (format t "~%Input: ~d~&Output:~&~d~%" in (static-network-output! network in)))))

(defun demo-page-336 ()
  "Chinese Edition, this example demonstrates the linear least squares utilized to set the parameters in layer2.
note that there are differences between the results of this demo with those in the textbook, and they are due to the instability of the inverse of the matrix"
  (let* ((network (make-static-network :neurons (list 1 3 1)
                                       :weights (list '((-2) (0) (2)))
                                       :biases (list '((0.5) (0.5) (0.5)))
                                       :input-proc (list :|dist| :*)
                                       :bias-proc (list :.* :+)
                                       :transfers (list #'radbas #'purelin)))
         (inputs '(-2 -1.2 -0.4 0.4 1.2 2))
         (targets '(0 0.19 0.69 1.3 1.8 2))
         (rdf-layer-id 0)
         (lin-layer-id 1)
         (neuron-outputs (loop for in in inputs
                               collect (let ((output (static-network-output! network in)))
                                         (declare (ignore output))
                                         (get-neuron-outputs network rdf-layer-id))))
         (optimal-parameters (linear-least-squares neuron-outputs targets)))
    (format t "Initial network:~&~d~%~%" network)
    (format t "Optimal parameters: ~d~%" optimal-parameters)
    (set-layer-parameters-from-list network lin-layer-id (first (transpose optimal-parameters)))
    network))

(defun example-P16.1-page-344 ()
  "Chinese Edition, an example of OLS algorithm.
It is note that when reading numbers(regressian matrix) as floats, either single or double, there would be the case that the stopping creteria 1 - Sigma(o_j) < 0 (about -0.05) after calculated all o's, but when treating them as rational numbers, the results were precisely 1 - Sigma(o_j) = 0.
"
  (let* (#+:ignore(network (make-static-network :neurons (list 1 5 1)
                                       ;;:weights (list '((-2) (0) (2)))
                                       ;;:biases (list '((0.5) (0.5) (0.5)))
                                       :input-proc (list :|dist| :*)
                                       :bias-proc (list :.* :+)
                                       :transfers (list #'radbas #'purelin)))
         (inputs '(-1 -0.5 0 0.5 1))
         (target-vector (transpose '((-1 0 1 0 -1))))
         #+:ignore(reg-matrix (transpose '((1.000 0.779 0.368 0.105 0.018)
                                  (0.779 1.000 0.779 0.368 0.105)
                                  (0.368 0.779 1.000 0.779 0.368)
                                  (0.105 0.368 0.779 1.000 0.779)
                                  (0.018 0.105 0.368 0.779 1.000)
                                  (1.000 1.000 1.000 1.000 1.000))))
         #+:ignore(reg-matrix (transpose '((1        779/1000 368/1000 105/1000  18/1000)
                                  (779/1000 1        779/1000 368/1000 105/1000)
                                  (368/1000 779/1000 1        779/1000 368/1000)
                                  (105/1000 368/1000 779/1000 1        779/1000)
                                  (18/1000  105/1000 368/1000 779/1000 1)
                                           (1        1        1        1        1))))
         (reg-matrix (make-ols-regression-matrix inputs))
         (ols-result (orthogonal-least-squares reg-matrix target-vector 0.05)))
    (format t "OLS result:~&~d~%" ols-result)
    ))

(defun example-P16.1-page-344-plus (&optional (inputs '(-1 -0.5 0 0.5 1)))
  "the first steps are the same as the previous example, but this example will update the parameters after ols,
and return the network with new parameters"
  (let* ((target-vector (transpose '((-1 0 1 0 -1))))
         (input-rank (if (numberp (first inputs))
                                1
                                (length (first inputs))))
         (reg-matrix (make-ols-regression-matrix inputs))
         (ols-result (orthogonal-least-squares reg-matrix target-vector 0.05))
         (bf-collector (first ols-result))
         (optimal-parameter-plist (second ols-result))
         (optimal-parameters (loop for i from 0 below (/ (length optimal-parameter-plist) 2) ;sort by property
                                   collect (getf optimal-parameter-plist i)))
         (weight-layer-rbf (loop for bf in bf-collector
                                 append (transpose (fourth bf))))
         (bias-layer-rbf (matrix-product (make-ones (length weight-layer-rbf) 1)
                                         (calc-bias weight-layer-rbf)))
         (weight-layer-lin (make-layer-weights-from-list optimal-parameters
                                                         1 ;currently only for one neuron
                                                         (car (matrix-size weight-layer-rbf))))
         (bias-layer-lin (if (> (- (length optimal-parameters) (matrix-elements-num weight-layer-lin)) 0)
                             (make-layer-biases-from-list (nthcdr (matrix-elements-num weight-layer-lin)
                                                                  optimal-parameters)
                                                          1))))
    (make-static-network :neurons (list input-rank (length weight-layer-rbf) (length weight-layer-lin))
                         :weights (list weight-layer-rbf weight-layer-lin)
                         :biases (list bias-layer-rbf bias-layer-lin)
                         :input-proc (list :|dist| :*)
                         :bias-proc (list :.* :+)
                         :transfers (list #'radbas #'purelin))
    ))

(defun exercise-E16.3-page-349 ()
  (let* ((network (make-static-network :neurons (list 1 2 1)
                                       :weights (list '((-1) (1)))
                                       :biases (list '((0.5) (0.5)) '((0)))
                                       :input-proc (list :|dist| :*)
                                       :bias-proc (list :.* :+)
                                       :transfers (list #'radbas #'purelin)))
         (inputs '(1 0 -1))
         (targets '(-1 0 1))
         (rdf-layer-id 0)
         (lin-layer-id 1)
         (neuron-outputs (loop for in in inputs
                               collect (static-network-partial-output network in rdf-layer-id)))
         ;;(rho 0)
         (rho 4)
         (optimal-parameters (linear-least-squares neuron-outputs targets rho)))
    (format t "Initial network:~&~d~%~%" network)
    (format t "Optimal parameters: ~d~%" optimal-parameters)
    (set-layer-parameters-from-list network lin-layer-id (first (transpose optimal-parameters)))
    network))
