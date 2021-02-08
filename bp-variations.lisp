(in-package #:nnd)

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
