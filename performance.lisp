(in-package #:nnd)

;;;; Chapter 9 Performance Optimization
;;;; Three different categories of optimization algorithm:
;;;; Steepest descent, Newton’s method and conjugate gradient

(defun stationary-point-p (hessian d point &optional (epsilon 0.00001))
  "check if point is a stationary point, when A x = d is an zero vevtor, return true"
  (when (< (norm (matrix-add (matrix-product hessian point) d)) epsilon) t))

(defgeneric steepest-descent (performance-index guessed-point &optional alpha iter-times iter-limit epsilon)
  (:documentation
   "Suppose that the performance index is a quadratic function: 
           F(x) = 1/2 x' A x + d' x + c
    x(k+1) = x(k) - α(k)g(k) 
    α(k) = -(g(k)' p(k)) / (p(k)' A p(k))
    where g(k) is the gradient for the k-th iteration, p(k) = -g(k)
    the algorithm stops when meets a convergence
    performance-index: the quadratic function, F(x) = 1/2 x' A x + d' x + c
    guessed-point: the initial random point
    alpha: the learning rate, which is a small positive fixed number, 
           or if provided with nil, will calc the best value so that it minimize the performance index, 
           alpha(k) = -1 * (gradient(k)' p(k)) / (p(k)' A(k) p(k)), where A(k) is the Hessian matrix.
  "))

(defmethod steepest-descent ((performance-index list) (guessed-point list)
                            &optional (alpha nil) (iter-times 0) (iter-limit 10000) (epsilon 0.00001))
  "performance-index, a plist denote the function, has the form: '(:A a-matrix :d a-column-vector :c a-number)
   guessed-point, a column vector, eg. '((x1) (x2)), if guessed-point is provided as nil, assign it with a random point
  "
  ;; test, p144: (steepest-escent '(:A ((10 -6) (-6 10)) :d ((4) (4)) :c 0) '((0) (-2)) nil)
  ;; test, p135: (steepest-escent '(:A ((2 1) (1 2)) :d ((0) (0)) :c 0) '((0.8) (-0.25)) nil)
  ;; test, p145: (steepest-escent '(:A ((10 2) (2 4)) :d ((-2) (-1)) :c 0.25) '((1) (1)) 0.05)
  (let* ((hessian  (getf performance-index :A))
         (d-vec    (getf performance-index :d))
         (gradient (matrix-add (matrix-product hessian guessed-point) d-vec))
         (p        (matrix-multiple-scalar gradient -1)) ;direction
         (point    (if guessed-point
                       guessed-point
                       (rand-matrix (length (first hessian)) 1)))
         (new-alpha (if (numberp alpha)
                        alpha
                        (* -1 (/ (inner-product gradient p)
                                 (inner-product p (matrix-product hessian p))))))
         (new-point (matrix-sub point (matrix-multiple-scalar gradient new-alpha)))
         (iter-times (1+ iter-times))
         (delta-x-norm (norm (matrix-sub new-point point))))
    (if (or (< delta-x-norm epsilon) (stationary-point-p hessian d-vec new-point epsilon))
        (progn (format t "~&Converged! Iteration times: ~d, ||Delta(x)||=~f~%" iter-times delta-x-norm)
               (format t "~&Converged to point:~%x = ~%")
               (print-matrix new-point)
               (format t "~&Performance index: ~d~%" (quadratic-function-value performance-index new-point)))
        (if (> iter-times iter-limit)
            (progn (format t "&The iteration was still not converged after ~d iterations, ||Delta(x)||=~f~%"
                           iter-times delta-x-norm))
            (progn (format t "~&NOT Converged! Iteration times: ~d, ||Delta(x)||=~f~%" iter-times delta-x-norm)
                   (format t "~&Update point,x = ~%")
                   (print-matrix new-point)
                   (steepest-descent performance-index new-point alpha iter-times iter-limit epsilon))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric newtons-method (performance-index guessed-point &optional alpha iter-times iter-limit epsilon)
  (:documentation
   "Newton’s method is based on the second-order Taylor series:
        F(x_k+1) = F(x_k + delta(x_k)) ≈ F(x_k) + gradient_k' delta(x_k) + 1/2 delta(x_k)' A_k delta(x_k)
    The principle behind Newton’s method is to locate the stationary point of this quadratic approximation to F(x).
    As gradient(F(x)) = Ax + d, taking the gradient of this quadratic function with respect to and set it equal to zero, we get
    gradient_k + A_k delta(x_k) = 0, so
    the updating method is:
        x_k+1 = x_k - inv(A_k) gradient_k
   "))

(defmethod newtons-method ((performance-index list) (guessed-point list)
                           &optional (alpha nil) (iter-times 0) (iter-limit 10000) (epsilon 0.00001))
  "performance-index, a plist denote the function, has the form: '(:A a-matrix :d a-column-vector :c a-number)
   guessed-point, a column vector, eg. '((x1) (x2)), if guessed-point is provided as nil, assign it with a random point
  "
  ;; test (newtons-method '(:A ((10 -6) (-6 10)) :d ((4) (4)) :c 0) '((0) (-2)) nil)
  (let* ((hessian (getf performance-index :A)))
    (if (= (det hessian) 0)
        (format t "~&Hessian matrix is invertable, cannot use Newton's Method.~%")
        (progn
          (let* ((d-vec       (getf performance-index :d))
                 (gradient    (matrix-add (matrix-product hessian guessed-point) d-vec))
                 (hessian-inv (matrix-inverse hessian))
                 (point       (if guessed-point
                                  guessed-point
                                  (rand-matrix (length (first hessian)) 1)))
                 (new-point (matrix-sub point (matrix-product hessian-inv gradient)))
                 (iter-times (1+ iter-times))
                 (delta-x-norm (norm (matrix-sub new-point point))))
            (if (or (< delta-x-norm epsilon) (stationary-point-p hessian d-vec new-point epsilon))
                (progn (format t "~&Converged! Iteration times: ~d, ||Delta(x)||=~f~%" iter-times delta-x-norm)
                       (format t "~&Converged to point:~%x = ~%")
                       (print-matrix new-point)
                       (format t "~&Performance index: ~d~%" (quadratic-function-value performance-index new-point)))
                (if (> iter-times iter-limit)
                    (progn (format t "&The iteration was still not converged after ~d iterations, ||Delta(x)||=~f~%"
                                   iter-times delta-x-norm))
                    (progn (format t "~&NOT Converged! Iteration times: ~d, ||Delta(x)||=~f~%" iter-times delta-x-norm)
                           (format t "~&Update point,x = ~%")
                           (print-matrix new-point)
                           (newtons-method performance-index new-point alpha iter-times iter-limit epsilon)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric conjugate-gradient (performance-index guessed-point &optional p-k-1 g-k-1 alpha iter-times iter-limit epsilon)
  (:documentation
   "Suppose that we wish to locate the minimum of the following quadratic function:
          F(x) = 1/2 x' A x + d' x + c
    gradient: g_k = A x + d
    Hessian matrix = A
    p_0 = -g_0
    p_k = -g_k + beta_k * p_k-1
    beta_k = (g_k g_k) / (g_k-1 g_k-1), where (a b) is the inner product of vector a and b
    x_k+1 = x_k + alpha_k * p_k
    alpha_k = -1 * (g_k' p_k) / (p_k' A_k p_k)
    
    p-k-1 is p_k-1, and g-k-1 is g_k-1
   "))

(defmethod conjugate-gradient ((performance-index list) (guessed-point list)
                               &optional (p-k-1 nil) (g-k-1 nil) (alpha nil) (iter-times 0) (iter-limit 10000) (epsilon 0.00001))
    "performance-index, a plist denote the function, has the form: '(:A a-matrix :d a-column-vector :c a-number)
     guessed-point, a column vector, eg. '((x1) (x2)), if guessed-point is provided as nil, assign it with a random point
    "
  ;; test, p145: (conjugate-gradient '(:A ((10 2) (2 4)) :d ((-2) (-1)) :c 0.25) '((1) (1)))
  ;; test, p141: (conjugate-gradient '(:A ((2 1) (1 2)) :d ((0) (0)) :c 0) '((0.8) (-0.25)))
  ;; test, p149: (conjugate-gradient '(:A ((10 2) (2 4)) :d ((-2) (-1)) :c 0.25) '((1) (1)))
  ;; test, p149: (conjugate-gradient '(:A ((10 2) (2 4)) :d ((-2) (-1)) :c 0.25) nil)
  (let* ((hessian (getf performance-index :A))
         (d-vec   (getf performance-index :d))
         (point   (if guessed-point
                      guessed-point
                      (rand-matrix (length (first hessian)) 1)))
         (g-k     (matrix-add (matrix-product hessian point) d-vec))
         (beta-k  (when (> iter-times 0) (/ (inner-product g-k g-k) (inner-product g-k-1 g-k-1))))
         (p-k     (if p-k-1
                      (matrix-sub (matrix-multiple-scalar p-k-1 beta-k) g-k)
                      (matrix-multiple-scalar g-k -1))) ;p_0 = -g_0
         (new-alpha (if (numberp alpha)
                        alpha
                        (* -1 (/ (inner-product g-k p-k)
                                 (inner-product p-k (matrix-product hessian p-k))))))
         (new-point (matrix-add point (matrix-multiple-scalar p-k new-alpha)))
         (iter-times (1+ iter-times))
         (delta-x-norm (norm (matrix-sub new-point point))))
    (if (or (< delta-x-norm epsilon) (stationary-point-p hessian d-vec new-point epsilon))
        (progn (format t "~&Converged! Iteration times: ~d, ||Delta(x)||= ~f~%" iter-times delta-x-norm)
               (format t "~&Converged to point:~%x = ~%")
               (print-matrix new-point)
               (format t "~&Performance index: ~d~%" (quadratic-function-value performance-index new-point)))
        (if (> iter-times iter-limit)
            (progn (format t "&The iteration was still not converged after ~d iterations, ||Delta(x)||=~f%"
                           iter-times delta-x-norm))
            (progn (format t "~&NOT Converged! Iteration times: ~d, ||Delta(x)||=~f~%" iter-times delta-x-norm)
                   (format t "~&Update point,x = ~%")
                   (print-matrix new-point)
                   (conjugate-gradient performance-index new-point p-k g-k alpha iter-times iter-limit epsilon))))))
