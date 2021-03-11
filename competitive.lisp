(in-package #:nnd)

;;;; Chapter 15, Dynamic Networks


(defmethod competitive-learning ((network static-network) samples &optional (learning-rate 0.25))
  "w(q) = w(q-1) + alpha * (p(q) - w(q-1)) for the winning neuron. the weights of the compet layer should be normalized first.
samples is a list of sample of input vectors"
  (with-slots ((weights weights)) network
    (dolist (sample samples)
      (let* ((input (first sample))
             (compet-layer-weight (first weights))
             (compet-result (static-network-output network input)))
        (setf compet-layer-weight
              (loop for cpt-res in (car (transpose compet-result))
                    for neuro-weight in compet-layer-weight
                    collect (if (= cpt-res 0)
                                neuro-weight
                                (let ((weight-row (list neuro-weight))
                                      (input-row (transpose input)))
                                  (first (matrix-add (matrix-product (- 1 learning-rate) weight-row)
                                                     (matrix-product learning-rate input-row)))))))
        (format t "~%input: ~d~&weights updated:~&~{~d~^~&~}" (transpose input) compet-layer-weight)
        )))
  network)

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
