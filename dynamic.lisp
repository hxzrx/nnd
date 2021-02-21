;;;; Chapter 15, Dynamic Networks

(in-package :nnd)

(defun FIR-demo ()
  "Finite Impulse Response Network Demonstration. Page 272, Chinese edition."
  (let ((tdl (make-tapped-delay-line 3 :init-element 0))
        (weights '((1/3 1/3 1/3)))
        (bias 0)
        (transfer #'purelin))
    (loop for i from 0 to 19
          do (progn
               (if (= (mod (- i (mod i 5)) 2) 0) ;generate square-wave input signal
                   (input-tdl tdl 1)
                   (input-tdl tdl -1))
               (let ((net-output (funcall transfer
                                (matrix-add (matrix-product
                                             weights
                                             (list-to-vector (get-tdl-contents tdl)))
                                            bias))))
                 (format t "~&~d~d~,3f~%" i #\tab net-output))))))


(defun IIR-demo ()
  "Infinite Impulse Response Network Demonstration"
  (let ((tdl (make-tapped-delay-line 2 :init-element 0))
        (weights '((1/2 1/2)))
        (bias 0)
        (transfer #'purelin))
    (loop for i from 0 to 19
          do (progn
               (if (= (mod (- i (mod i 5)) 2) 0) ;generate square-wave input signal
                   (input-tdl tdl 1)
                   (input-tdl tdl -1))
               (let ((net-output (funcall transfer
                                (matrix-add (matrix-product
                                             weights
                                             (list-to-vector (get-tdl-contents tdl)))
                                            bias))))
                 (input-tdl tdl net-output) ; the difference between fir-demo
                 (format t "~&~d~d~,3f~%" i #\tab net-output))))))
