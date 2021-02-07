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

(setf pfun1 (quadratic-function '((2 1) (1 2))))
(interval-location pfun1 '((0.8) (-0.25)) '((-1.35) (-0.3)) 0.075)


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
(setf pfun2 (quadratic-function '((2 1)(1 2))))                    
(golden-section-search pfun2 '((0.8) (-0.25)) '((-1.35) (-0.3)) 0.15 0.6)
|#
