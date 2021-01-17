(in-package #:nnd)

(defgeneric hebb-rule (samples p)
 (:documentation "for samples {p1,t1}, {p2,t2}, ..., {pQ,tQ}
                  W = t1p1' + t2p2' + ... + tqpQ'
                  the result is Wp"))

(defmethod hebb-rule ((samples list) (p list))
  "linear associator, a = Wp, or a = purelin(Wp)
   W = t1p1' + t2p2' + ... + tqpQ', returb Wp.
   pi and ti in each sample has the row form, eg. '((1 2) (1 1)), for simplicity.
   the test vector p may be either a row vector or a column vector.
   the result is a column vector.
   hebb rule has a correct result when the input patterns are orthogonal
  "
  ;;(hebb-rule '(((1/2 -1/2 1/2 -1/2) (1 -1)) ((1/2 1/2 -1/2 -1/2) (1 1)) ) '((1/2 -1/2 1/2 -1/2)))
  (purelin (matrix-product (reduce #'matrix-add
                          (loop for sample in samples
                                collect (matrix-product (transpose (list (second sample))) ;transfer to column vector
                                                        (list (first sample)))))
                  (if (row-vector-p p)
                      (transpose p)
                      p))))

(defgeneric pseudoinverse-hebb-rule (samples test-vector)
  (:documentation
   "When the prototype input patterns are not orthogonal, the Hebb rule produces some errors. So we use the pseudoinverse rule.
    When the number of rows is greater than the number of columns, and the columns of are independent, then the pseudoinverse can
    be computed by: P+ = (P'P)^(-1) P'
    Here, W = TP+, P+ = (P'P)^(-1) P'"))

(defmethod pseudoinverse-hebb-rule ((samples list) (test-vector list))
  "W = TP+, P+ = (P'P)^(-1) P', this method is not functional"
  ;; test: (pseudoinverse-hebb-rule '(((1 -1 -1) (-1)) ((1 1 -1) (1))) '((1 -1 -1)))
  (let ((T-rev nil)
        (P-rev nil))
    (loop for sample in samples
          do (progn (push (first  sample) P-rev)
                    (push (second sample) T-rev)))
    (let* ((P  (transpose (reverse P-rev)))
           (T-matrix  (transpose (reverse T-rev)))
           (P+ (matrix-product (matrix-inverse (matrix-product (transpose P) P))
                               (transpose P)))
           (W (matrix-product T-matrix P+)))
      (format t "~&T, P+ and Weight:~%")
      (print-matrix T-matrix)
      (print-matrix P+)
      (print-matrix W)
      (purelin (matrix-product W
                               (if (row-vector-p test-vector)
                                   (transpose  test-vector)
                                   test-vector))))))
                               
(defgeneric autoassociative (samples test-vector)
  (:documentation
   "when the output vector is the same as the input vector, W = p1p1' + ... + pQpQ'
    a = hardlims(Wp)
   "))

(defmethod autoassociative ((samples list) (test-vector list))
  "W = p1p1' + ... + pQpQ'
   samples: '((1 1 1 1) (-1 -1 -1 -1) (-1 1 1 1) ...)"
  ;; test (autoassociative '((1 1 -1 1 -1 -1) (-1 1 1 1 1 -1)) '((1 1 1 1 1 -1)))
  (hardlims (matrix-product
             (reduce #'matrix-add (loop for input in samples
                                        collect (matrix-product (transpose (list  input))
                                                                (list input))))
             (if (row-vector-p test-vector)
                 (transpose  test-vector)
                 test-vector))))

  
