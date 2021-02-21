;;;; Chapter 15, Dynamic Networks

(in-package :nnd)

(defun FIR-demo ()
  "Finite Impulse Response Network Demonstration. Page 272, Chinese edition."
  (let ((tdl (make-tapped-delay-line 3 :init-element 2))
        (weights '((1/3 1/3 1/3)))
        (bias 0)
        (transfer #'purelin))
    (loop for i from 1 to 20
          do (progn
               (if (= (mod (- i (mod i 5)) 2) 0)
                   (input-tdl tdl 1)
                   (input-tdl tdl -1))
               (format t "~&~d~d~d~%"
                       i
                       #\tab
                       (funcall transfer
                               (matrix-add (matrix-product weights
                                                           (list-to-vector (get-tdl-contents tdl)))
                                             bias)))))))
