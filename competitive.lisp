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

(defgeneric kohonen-update-neighbor (old-weight input compet-result &key neuron-arranged radius learning-rate)
  (:documentation "Kohonen rule: w(q) = w(q-1) + alpha * (p(q) - w(q-1)) for the neurons that are neighbors of the winning neuro.
In SOFM, neuros can be arranged as an m * n matrix. `neuro-arranged' ia a list (list m n) and m*n should eql to the length of `old-weight'")
  (:method ((old-weight list) (input list) (compet-result list) &key neuron-arranged (radius 0) (learning-rate 0.5))
    "the generic `kohonen-update' is a special case when radius=0"
    (let* ((center (nth-win compet-result))
           (neurons (length old-weight))
           (arranged (if neuron-arranged
                         neuron-arranged
                         (list neurons 1)))
           (rad (if radius radius 1))
           (update-neurons (neighbor-id neurons arranged center rad)))
      (format t "Updating neurons: ~{~d~^ ~}~%" update-neurons)
      (loop for neuron-weight in old-weight
            for i from 0
            collect (if (member i update-neurons)
                        (first (matrix-add (matrix-product (- 1 learning-rate) (list neuron-weight))
                                           (matrix-product learning-rate (transpose input))))
                        neuron-weight)))))

(defgeneric kohonen-update-lvq (old-weight input compet-result correctly-classified? learning-rate)
  (:documentation "Kohonen rule:
If `input' was   correctly classified, w(q) = w(q-1) + alpha * (p(q) - w(q-1)) for the winning neuron.
If `input' was incorrectly classified, w(q) = w(q-1) - alpha * (p(q) - w(q-1)) for the winning neuron.")
  (:method ((old-weight list) (input list) compet-result correctly-classified? learning-rate)
    "`old-weight' is a weight matrix and `input' is a column vector"
    (loop for cpt-res in (car (transpose compet-result))
          for weight in old-weight
          collect (if (< (abs (- cpt-res 1)) 0.00000001)
                      (let ((weight-row (list weight)) ;winner
                            (input-row (transpose input)))
                        (if correctly-classified?
                            (first (matrix-add (matrix-product (- 1 learning-rate) weight-row)
                                               (matrix-product learning-rate input-row)))
                            (first (matrix-sub (matrix-product (- 1 learning-rate) weight-row)
                                               (matrix-product learning-rate input-row)))))
                      weight))))

(defun nth-win (compet-result &optional (n 0))
  "compet-result is a column vector with one element is one and else elements are zeros"
  (if compet-result
      (if (= (caar compet-result) 1)
          n
          (nth-win (cdr compet-result) (1+ n)))
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; competitive learning

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
               (compet-result (static-network-output! network input))
               (updated-weights (kohonen-update compet-layer-weight input compet-result learning-rate)))
          (format t "~&Q=~d, compet result: ~{~d~^ ~}~%Weight:~%~{~{~d~^ ~}~^~&~}~%" time compet-result compet-layer-weight)
          ;;CANNOT setf compet-layer-weight, that will leading to unpredictable results
          (setf (nth compet-layer-id weights) updated-weights)
          (format t "~%Updated to:~%~{~{~d~^ ~}~^~&~}~%~%" (nth compet-layer-id weights))
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
                 (compet-result (static-network-output! network input))
                 (updated-weights (kohonen-update compet-layer-weight input compet-result learning-rate))
                 (updated-biases (conscious-update compet-layer-bias compet-result)))
            (format t "~&Q=~d, compet result: ~{~d~^ ~}~%Weight:~%~{~{~d~^ ~}~^~&~}~&Biases:~&~{~{~d~^ ~}~^~&~}~%" time compet-result compet-layer-weight compet-layer-bias)
            ;;CANNOT setf compet-layer-weight, that will leading to unpredictable results
            (setf (nth compet-layer-id weights) updated-weights)
            (setf (nth compet-layer-id biases) updated-biases)
            (format t "~%Updated to:~&weights:~%~{~{~d~^ ~}~^~&~}~&Updated biases:~%~{~{~d~^ ~}~^~&~}~%~%" (nth compet-layer-id weights) (nth compet-layer-id biases))
            ))))
    (format t "Trained network: ~d~%" network)
    network))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;SOFM

(defmethod sofm-learning ((network static-network) samples &optional neuron-arranged (learning-rate 0.5) (radius 1))
  "basic Self-Organizing Feature Maps learning"
    (with-slots ((weights weights)
                 (neurons neurons)) network
      (let* ((compet-layer-id 0)
             (time 0)
             (arranged (if neuron-arranged
                           neuron-arranged
                           (list (nth (1+ compet-layer-id) neurons)))))
        (dolist (sample samples)
          (incf time)
          (let* ((input (first sample))
                 (compet-layer-weight (first weights))
                 (compet-result (static-network-output! network input))
                 (updated-weights (kohonen-update-neighbor compet-layer-weight input compet-result
                                                           :learning-rate learning-rate
                                                           :radius radius
                                                           :neuron-arranged  arranged)))
            (format t "~&Q=~d, compet result: ~{~d~^ ~}~%Weight:~%~{~{~d~^ ~}~^~&~}~%" time compet-result compet-layer-weight)
            ;;CANNOT setf compet-layer-weight, that will leading to unpredictable results
            (setf (nth compet-layer-id weights) updated-weights)
            (format t "~%Updated to:~%~{~{~d~^ ~}~^~&~}~%~%" (nth compet-layer-id weights))
            ))))
  (format t "Trained network: ~d~%" network)
  network)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Learning Vector Quantization

(defmethod lvq-learning ((network static-network) samples &optional (learning-rate 0.5))
  "Learning Vector Quantization"
    (with-slots ((weights weights)
                 (neurons neurons)
                 (neuron-outputs neuron-outputs)) network
      (let* ((compet-layer-id 0)
             (time 0))
        (dolist (sample samples)
          (incf time)
          (let* ((input (first sample))
                 (target (second sample))
                 (compet-layer-weight (nth compet-layer-id weights))
                 (classify-result (static-network-output! network input))
                 (compet-result (nth compet-layer-id neuron-outputs))
                 (correctly-classiefied? (equal target classify-result))
                 (updated-weights (kohonen-update-lvq compet-layer-weight input compet-result
                                                      correctly-classiefied? learning-rate)))
            (format t "~&Q=~d, compet result: ~{~d~^ ~}~%Correctly classified: ~d~%Weight:~%~{~{~d~^ ~}~^~&~}~%" time compet-result correctly-classiefied? compet-layer-weight)
            ;;CANNOT setf compet-layer-weight, that will leading to unpredictable results
            (setf (nth compet-layer-id weights) updated-weights)
            (format t "~%Updated to:~%~{~{~d~^ ~}~^~&~}~%~%" (nth compet-layer-id weights))
            ))))
  (format t "Trained network: ~d~%" network)
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

(defun example-p15.4-page-318 (&optional (sample-repeats 1))
  "SOFM example"
  (let ((network (make-static-network :neurons (list 3 9)
                                      :weights (list '((0.41 0.41 0.82) (0.45 0 0.89) (0.41 -0.41 0.82) (0 0.45 0.89) (0 0 1)
                                                       (0 -0.45 0.89) (-0.41 0.41 0.82) (-0.45 0 0.89) (-0.41 -0.41 0.82)))
                                      :summers (list :sum)
                                      :transfers (list #'compet)))
        (learning-rate 0.1)
        (radius 1)
        (neuron-arranged (list 9 1))
        (samples (repeat-list (list (list '((0.67) (0.07) (0.74))))
                              sample-repeats))
        )
    (format t "Initial network:~&~d~%~%" network)
    (with-slots ((weights weights)) network ;normalize the compet layer's weight
      (let* ((compet-layer-weight (first weights)))
        (setf compet-layer-weight
              (loop for rows in compet-layer-weight
                    collect (first (normalize (list rows)))))))
    (sofm-learning network samples neuron-arranged learning-rate radius)
    ))

(defun example-lvq-page-318 (&optional (sample-repeats 1))
  "LVQ example"
  (let ((network (make-static-network :neurons (list 2 4 2)
                                      :weights (list '((-0.543 0.84) (-0.969 -0.249) (0.997 0.094) (0.456 0.954))
                                                     '((1 1 0 0) (0 0 1 1)))
                                      :summers (list :dist :sum)
                                      :transfers (list #'compet #'purelin)))
        (learning-rate 0.5)
        (samples (repeat-list (list (list '((1) (-1)) '((0) (1))))
                              ;;(list '((-1) (-1)) '((1) (0)))
                              ;;(list '((1) (1)) '((1) (0)))
                              ;;(list '((-1) (1)) '((0) (1))))
                              sample-repeats))
        )
    (format t "Initial network:~&~d~%~%" network)
    (lvq-learning network samples learning-rate)
    ))
