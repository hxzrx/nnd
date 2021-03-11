(in-package #:nnd)

;;;; Chapter 15, Dynamic Networks


(defmethod competitive-learning ((network static-network) samples &optional (learning-rate 0.5))
  "Kohonen rule: w(q) = w(q-1) + alpha * (p(q) - w(q-1)) for the winning neuron.
the weights of the compet layer should be normalized first. samples is a list of sample of input vectors"
  (let ((compet-layer-id 0)
        (time 0))
    (with-slots ((weights weights)) network
      (dolist (sample samples)
        (incf time)
        (let* ((input (first sample))
               (compet-layer-weight (first weights))
               (compet-result (static-network-output network input))
               (updated-weights (loop for cpt-res in (car (transpose compet-result))
                                      for neuro-weight in compet-layer-weight
                                      collect (if (< (abs (- cpt-res 1)) 0.00000001)
                                                  (let ((weight-row (list neuro-weight))
                                                        (input-row (transpose input)))
                                                    (first (matrix-add (matrix-product (- 1 learning-rate) weight-row)
                                                                       (matrix-product learning-rate input-row))))
                                                  neuro-weight))))
          (format t "~&Q=~d, compet result: ~{~d~^ ~}~%Weight:~%~{~{~d~^ ~}~^~&~}~%" time compet-result compet-layer-weight)

          ;;CANNOT setf compet-layer-weight, that will leading to unpredictable results
          (setf (nth compet-layer-id weights) updated-weights)
          (format t "~&Updated to:~%~{~{~d~^ ~}~^~&~}~%~%" (nth compet-layer-id weights))
          ))))
  network)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; examples and exercises

(defun example-compet-learn-page-305 ()
  (let ((network (make-static-network :neurons (list 2 3)
                                      :weights (list '((0.7071 -0.7071) (0.7071 0.7071) (-1 0)))
                                      :summers (list :sum)
                                      :transfers (list #'compet)))
        (samples (list (list '((0.1961)  (0.9806))) ;in the textbook, p2 is the first input vector
                       (list '((-0.1961) (0.9806)))
                       (list '((0.9806)  (0.1961)))
                       (list '((0.9806)  (-0.1961)))
                       (list '((-0.5812) (-0.8137)))
                       (list '((-0.8137) (-0.5812))))))
    (format t "Initial network:~&~d~%~%" network)
    (with-slots ((weights weights)) network ;normalize the compet layer's weight
      (let* ((compet-layer-weight (first weights)))
        (setf compet-layer-weight
              (loop for rows in compet-layer-weight
                    collect (first (normalize (list rows)))))))
    (competitive-learning network samples 0.5)))

(defun example-p15.2-page-316 ()
  (let ((network (make-static-network :neurons (list 2 3)
                                      :weights (list (list (list 0 -1)
                                                           (list (/ -2 (sqrt 5)) (/ 1 (sqrt 5)))
                                                           (list (/ -1 (sqrt 5)) (/ 2 (sqrt 5)))))
                                      :summers (list :sum)
                                      :transfers (list #'compet)))
        (samples (repeat-list (list (list '((-1) (0)))
                                      (list '((0) (1)))
                                      (list (list (list (/ 1 (sqrt 2))) (list (/ 1 (sqrt 2))))))
                                2)
                       ))
    (format t "Initial network:~&~d~%~%" network)
    (with-slots ((weights weights)) network ;normalize the compet layer's weight
      (let* ((compet-layer-weight (first weights)))
        (setf compet-layer-weight
              (loop for rows in compet-layer-weight
                    collect (first (normalize (list rows)))))))
    (competitive-learning network samples 0.5)))

(defun exercise-e15.3-page-323 ()
  (let ((network (make-static-network :neurons (list 2 2)
                                      :weights (list (list (list (sqrt 2) 0)
                                                           (list 0 (sqrt 2))))
                                      :summers (list :sum)
                                      :transfers (list #'compet)))
        (samples (list (list '((1) (-1)))
                       (list '((1) (1)))
                       (list '((-1) (-1)))
                       )))
    (format t "Initial network:~&~d~%~%" network)
    (with-slots ((weights weights)) network ;normalize the compet layer's weight
      (let* ((compet-layer-weight (first weights)))
        (setf compet-layer-weight
              (loop for rows in compet-layer-weight
                    collect (first (normalize (list rows)))))))
    (competitive-learning network samples 0.5)))
