(in-package #:nnd)

;;;; Chapter 16 (Chinese Edition), Radial Basis Networks

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
  (format t "~%<calc-initial-basis>~&")
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
                      (format t "~&loop i: ~d, mᵀm: ~d, h: ~d, tᵀt: ~d, o: ~d~%" i mᵀm h tᵀt o)
                      (push (list 0 i o u) bf-collector))))
    (setf bf-collector (reverse bf-collector))
    (format t "Init bf-collector: ~d~%" bf-collector)
    (format t "select result: ~d~%" (first (sort bf-collector #'> :key #'third)))
    (first (sort bf-collector #'> :key #'third))))

(defun sum-contributes% (bf-collect)
  (loop for (nil nil o nil) in bf-collect
        sum o))

(defun calc-inner-product-to-orthed-vectors (reg-matrix bf-collector)
  "calc $r_{j,k}^{(i)}$ for k = (length bf-collector), here, k is fixed, i and j are indices.
bf-collector was initialized in calc-initial-basis"
  (format t "~%<calc-inner-product-to-orthed-vectors>r_{j,k}^{(i)}~&")
  (let ((Q (length reg-matrix)))
        ;(k (length bf-collector)))
    (format t "~&bf-collector: ~d~%" bf-collector)
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
      (format t "res: ~d~&" res)
      res)))

(defun calc-candidate-orth-vectors (reg-matrix bf-collector r-collector)
  "calc $m_k^{(i)}$ for k = (length bf-collectr)"
  (format t "~%<calc-candidate-orth-vectors>m_k^{(i)}~&")
  (let ((Q (length reg-matrix)))
    ;;(k (length bf-collector)))
    ;;(format t "~&bf-collector: ~d~%" bf-collector)
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
      (format t "~&result: ~d~%" res)
      res)))

(defun calc-inner-product-to-target (candidate-orth-vecs target-vector)
    "calc $h_k^{(i)}$ for k = (length bf-collector), here, k is fixed, i is the index.
candidate-orth-vecs gets from calc-candidate-orth-vectors"
  (format t "~%<calc-inner-product-to-target>h_k^{(i)}~%")
  (format t "~&candidate:~&~d~&target-vec: ~d~%" candidate-orth-vecs target-vector)
  (let ((res (loop for (i m) in candidate-orth-vecs
                   collect
                   (list i (/ (inner-product m target-vector)
                              (inner-product m m))))))
    (format t "~&result: ~d~%" res)
    res))

(defun calc-contributes (candidate-orth-vecs h-collector target-vector)
  "calc $o_k^{(i)}$ for k = (length bf-collector), here, k is fixed, i is the index.
candidate-orth-vecs gets from calc-candidate-orth-vectors,
h-collector getf from calc-inner-product-to-target"
  (format t "~%<calc-contributes>o_k^{(i)}~%")
  ;;(format t "~&candidate-orth-vecs:~&~d~%" candidate-orth-vecs)
  ;;(format t "~&h-collector:~&~d~%" h-collector)
  (let* ((tᵀt (inner-product-self target-vector))
         (res
           (loop for (i h) in h-collector
                 collect (list i
                               (/ (* h h (inner-product-self (second (find i candidate-orth-vecs :key #'first)))) tᵀt)))))
    (format t "~&result: ~d~&" res)
    res))



(defun orthogonal-least-squares (reg-matrix target-vector &optional (delta 0.05))
  "this assumes that a target is a scalar, so targets is a list of numbers.
reg-matrix is U in the algorithm, target-vetor is t in the algorithm, t = Ux + e
"
  (let* (;(tᵀt (matrix-product (transpose target-vector) target-vector))
         (bf-collector (list (calc-initial-basis reg-matrix target-vector))) ; (list 0 i o u)
         (r-jk nil)
         (contribute-list (list (third (first bf-collector)))))
    (format t "~&Begin to loop k.~%")
    (loop for k from 1 to (length reg-matrix)
          do (progn (let* ((r (calc-inner-product-to-orthed-vectors reg-matrix bf-collector))
                           (m (calc-candidate-orth-vectors reg-matrix bf-collector r))
                           (h (calc-inner-product-to-target m target-vector))
                           (o (calc-contributes m h target-vector))
                           (o-sort (sort o #'> :key #'second))
                           (max-o (second (first o-sort)))
                           (max-o-index (first (first o-sort))) ;i_k
                           )
                      (format t "~%-----k = ~d---------~&" k)
                      ;(format t "~&r:~&~d~%" r)
                      ;(format t "~&m:~&~d~%" m)
                      ;(format t "~&h:~&~d~%" h)
                      ;(format t "~&o:~&~d~%" o)
                      ;(format t "~&max-o:~&~d~%" max-o)
                      ;(format t "~&max-o-index:~&~d~%" max-o-index)
                      (push max-o contribute-list)
                      ;(format t "~&contribute-list: ~d~%" contribute-list)
                      (loop for j from 0 to (- k 1)
                            do (push (list j k (third (find (list max-o-index j) r
                                                            :key #'(lambda (seq) (subseq seq 0 2))
                                                            :test #'equal)))
                                     r-jk))
                      (nconc bf-collector (list (list k max-o-index max-o (second (find max-o-index m :key #'first))) ))
                      (when (<= (- 1 (reduce #'+ contribute-list)) delta)
                        (format t "~&Get stop criteria: ~f, at k = ~f, delta = ~f~&contribute: ~{~f~^ ~}~%" (- 1 (reduce #'+ contribute-list)) k delta contribute-list)
                        (return-from nil)))))
    bf-collector))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; examples and exercises

(defun demo-page-330 ()
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
         ;;(inputs '(-1 -0.5 0 0.5 1))
         (target-vector (transpose '((-1 0 1 0 -1))))
         (reg-matrix (transpose '((1.000 0.779 0.368 0.105 0.018)
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
         ;;(rdf-layer-id 0)
         ;;(lin-layer-id 1)
         (ols-result (orthogonal-least-squares reg-matrix target-vector 1e-15)))
    ;;(format t "Initial network:~&~d~%~%" network)
    (format t "OLS result:~&~d~%" ols-result)
    ))
