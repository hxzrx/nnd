(in-package #:nnd)

;;;; Chapter 15, Dynamic Networks

(defgeneric kohonen-update (old-weight input compet-result learning-rate)
  (:documentation "Kohonen rule: w(q) = w(q-1) + alpha * (p(q) - w(q-1)) for the winning neuron and not changed for the failed")
  (:method ((old-weight list) (input list) compet-result learning-rate)
    "`old-weight' is a weight matrix and `input' is a column vector"
    (loop for cpt-res in (car (transpose compet-result))
          for weight in old-weight
          collect (if (< (abs (- cpt-res 1)) 0.00000001)
                      (let ((weight-row (list weight)) ;winner
                            (input-row (transpose input)))
                        (first (matrix-add (matrix-product (- 1 learning-rate) weight-row)
                                           (matrix-product learning-rate input-row))))
                      weight))))


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
               (updated-weights (kohonen-update compet-layer-weight input compet-result learning-rate)))
          (format t "~&Q=~d, compet result: ~{~d~^ ~}~%Weight:~%~{~{~d~^ ~}~^~&~}~%" time compet-result compet-layer-weight)
          ;;CANNOT setf compet-layer-weight, that will leading to unpredictable results
          (setf (nth compet-layer-id weights) updated-weights)
          (format t "~&Updated to:~%~{~{~d~^ ~}~^~&~}~%~%" (nth compet-layer-id weights))
          ))))
  (format t "Trained network: ~d~%" network)
  network)

(defun check-dead-neurons (old-weights new-weights)
  "a neuron is called deal if it's weight didn't change after training"
  (format t "old weights:~&~{~d~^~&~}~&new weights:~&~{~d~^~&~}~%" old-weights new-weights)
  (loop for w-old in old-weights
        for w-new in new-weights
        for index from 0
        when (equal w-old w-new)
          collect index))

(defgeneric conscious-update (old-biases compet-result &optional fail-factor win-sub epsilon)
  (:documentation "update biases utlizing conscious, page 324, E15.4 of the textbook")
  (:method ((old-biases list) (compet-result list) &optional (fail-shrink 0.9) (win-sub 0.2) (epsilon 0.00000001))
    (loop for cpt-res in (car (transpose compet-result))
          for neuro-bias in old-biases
          collect (if (< (abs (- cpt-res 1)) epsilon)
                      (list (- (car neuro-bias) win-sub)) ;winner
                      (list (* fail-shrink (car neuro-bias)))))))

(defmethod competitive-learning-consicious ((network static-network) samples &optional (learning-rate 0.5))
  "Kohonen rule: w(q) = w(q-1) + alpha * (p(q) - w(q-1)) for the winning neuron.
the weights of the compet layer should be normalized first. samples is a list of sample of input vectors"
  (let ((compet-layer-id 0)
        (old-weights (copy-seq (get-weights network)))
        (train-result (competitive-learning network samples learning-rate))
        (time 0))
    (alexandria:when-let (dead-neurons (check-dead-neurons (nth compet-layer-id old-weights)
                                                           (nth compet-layer-id (get-weights train-result))))
      (format t "~&Dead neurons found: ~{~d~^ ~}~&Conscious technique will be used next!~%" dead-neurons)
      (with-slots ((weights weights)
                   (biases biases)) network
      (dolist (sample samples)
          (incf time)
          (let* ((input (first sample))
                 (compet-layer-weight (first weights))
                 (compet-layer-bias   (first biases))
                 (compet-result (static-network-output network input))
                 (updated-weights (kohonen-update compet-layer-weight input compet-result learning-rate))
                 (updated-biases (conscious-update compet-layer-bias compet-result)))
            (format t "~&Q=~d, compet result: ~{~d~^ ~}~%Weight:~%~{~{~d~^ ~}~^~&~}~&Biases:~&~{~{~d~^ ~}~^~&~}~%" time compet-result compet-layer-weight compet-layer-bias)
            ;;CANNOT setf compet-layer-weight, that will leading to unpredictable results
            (setf (nth compet-layer-id weights) updated-weights)
            (setf (nth compet-layer-id biases) updated-biases)
            (format t "~&Updated weights:~%~{~{~d~^ ~}~^~&~}~&Updated biases:~%~{~{~d~^ ~}~^~&~}~%~%" (nth compet-layer-id weights) (nth compet-layer-id biases))
            ))))
    (format t "Trained network: ~d~%" network)
    network))







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
                                2))
        (learning-rate 0.5))
    (format t "Initial network:~&~d~%~%" network)
    (with-slots ((weights weights)) network ;normalize the compet layer's weight
      (let* ((compet-layer-weight (first weights)))
        (setf compet-layer-weight
              (loop for rows in compet-layer-weight
                    collect (first (normalize (list rows)))))))
    (competitive-learning network samples learning-rate)
    ))

(defun exercise-e15.4-page-324-conscious (&optional (sample-repeats 1))
  (let ((network (make-static-network :neurons (list 2 3)
                                      :weights (list (list (list 0 -1)
                                                           (list (/ -2 (sqrt 5)) (/ -1 (sqrt 5)))
                                                           (list (/ -1 (sqrt 5)) (/ -2 (sqrt 5)))))
                                      :summers (list :sum)
                                      :transfers (list #'compet)))
        (samples (repeat-list (list (list '((-1) (0)))
                                      (list '((0) (1)))
                                      (list (list (list (/ 1 (sqrt 2))) (list (/ 1 (sqrt 2))))))
                                sample-repeats))
        (learning-rate 0.5))
    (format t "Initial network:~&~d~%~%" network)
    (with-slots ((weights weights)) network ;normalize the compet layer's weight
      (let* ((compet-layer-weight (first weights)))
        (setf compet-layer-weight
              (loop for rows in compet-layer-weight
                    collect (first (normalize (list rows)))))))
    (competitive-learning-consicious network samples learning-rate)
    ))

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
