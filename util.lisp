(in-package #:nnd)

(setf *random-state* (make-random-state t))

;;;; utils for list and vector or matrix of type list
(defun list-length-equal (a b)
  "check if two lists have the same length"
  (cond  ((and (null a) (null b)) t)
         ((and (null a) b) nil)
         ((and a (null b)) nil)
         (t (list-length-equal (cdr a) (cdr b)))))

(defun list-check-type (lst type &optional (raw-nil lst))
  "check if all elements are of the specified type" ;用every对nil会返回t
  (if (and (null lst) raw-nil)
      t
      (and (typep (car lst) type) (list-check-type (cdr lst) type raw-nil))))

(defun lists-length-equal (lists &optional (rows 0) (cols 0))
  "check if the sublists of a list have the same length.
   return nil if lists is not rectangle, or return (rows . columns)"
  (cond ((= rows 0) (lists-length-equal (cdr lists) (length lists) (length (car lists))))
        ((null lists) (cons rows cols))
        (t (and (= cols (length (car lists)))
                (lists-length-equal (cdr lists) rows cols)))))

(defun matrixp (matrix)
  "check if it is a matrix"
  (if (and (list matrix) (every #'listp matrix))
      (lists-length-equal matrix)
      nil))

(defun lists-square-p (lists)
  "check the matrix is square, if square, return the size n, else return nil"
  (let ((size (lists-length-equal lists)))
    (when (and size (= (car size) (cdr size)))
      (car size))))

(defun matrix-square-p (matrix)
  "check the matrix is square, if square, return the size n, else return nil"
  (lists-square-p matrix))

(defun list-check-rectangle (lst)
  "check if each sublist in 'lst has the same length, return nil if not, or return (rows . columns)"
  (lists-length-equal lst))

(defun replace-col (matrix vec idx)
  "replace the idx column of matrix with vec, note, for simplitly, vec is a row vector, ((1 2 3))"
  (loop for row in matrix
        for b   in (transpose vec)
        collect (loop for element in row
                      for i from 0
                      collect (if (= i idx ) (car b) element))))

(defun zeros-under-pivot-p (matrix pivot-idx)
  "return t if all the elements below the nth pivot are zero, otherwise return nil"
  (let ((rows (car (matrix-size matrix))))
    (assert (> rows pivot-idx))
    (every #'(lambda (row) (= (nth pivot-idx row) 0)) (cdr (nthcdr pivot-idx matrix)))))

(defun posioning-with-predicate (lst &optional &key place tmp idx key test)
  "find the place in the lst at which pfun always returns true when compared with other element in the list,
   key and test parameter should be provided!
   eg. (posioning-with-predicate '(1 4 3 5 3) :key #'identity :test #'>)
       it returns 3, which is the place of element 5"
;;  (format t "~&place: ~d, tmp: ~d, idx: ~d, key: ~d, test: ~d" place tmp idx key test)
  (cond ((null place) (posioning-with-predicate (cdr lst) :place 0 :idx 0 :tmp (funcall key (car lst)) :key key :test test))
        ((null lst) place)
        (t (incf idx)
           (let ((cmp-elem (funcall key (car lst))))
             (if (funcall test cmp-elem tmp)
                 (posioning-with-predicate (cdr lst) :place idx   :idx idx :tmp cmp-elem :key key :test test)
                 (posioning-with-predicate (cdr lst) :place place :idx idx :tmp tmp      :key key :test test))))))

(defun posioning-greatest-pivot (matrix nth-pivot)
  "find the row id of the matrix that has the greast absolute value under the pivot place"
  (+ (posioning-with-predicate (nthcdr nth-pivot matrix) :key #'(lambda (x) (abs (nth nth-pivot x))) :test #'>)
     nth-pivot))

(defun zeros-row-p (lst)
  "check if lst is of all zeros"
  (if (null lst) t
      (and (= (car lst) 0) (zeros-row-p (cdr lst)))))

(defun zeros-p (matrix)
  "check if all of the elements are 0"
  (if (numberp matrix) (= matrix 0)
      (if (null matrix) t
          (and (zeros-row-p (car matrix)) (zeros-p (cdr matrix))))))

;;;; utils for transfer functions
(defun collect-transfer (trans-fun col-vec)
  "apply the transfer function to each of the column vector"
  (assert (= 1 (cdr (lists-length-equal col-vec))))
  (loop for row in col-vec
        collect (loop for elem in row
                      collect (funcall trans-fun elem))))

(defun print-matrix (m)
  "print matrix"
  (if (listp m)
      (format  t "~&~{~{~d  ~}~&~}" m)
      (format t "~&~d~%"m)))

(defun list-given-place-1-others-0 (lst k &optional (i 0) (epsilon 0.000001))
  "check if the kth place of lst is close to 1, and close to 0 otherwise
   lst has the form '(1 2 3), and 0 <= k <= (length lst)
  "
  (if (null lst) t
      (and (if (= i k)
               (< (abs (1- (car lst))) epsilon)
               (< (abs (car lst)) epsilon))
           (list-given-place-1-others-0 (cdr lst) k (1+ i) epsilon))))

(defun list-zeros-p (lst &optional (epsilon 0.000001))
  "check if the elements of lst were close to zero"
  (if (null lst) t
      (and (< (abs (car lst)) epsilon)
           (list-zeros-p (cdr lst) epsilon))))

(defun print-training-result (weights bias correct-rate)
  "print weight, bias, and correct-rate"
  (format t "~&Performance Index: ~f~%" correct-rate)
  (format t "~&Weights:~%")
  (print-matrix weights)
  (format t "~&Bias:~%")
  (print-matrix bias))

(defun quadratic-function-value (quadratic point)
  "calc the value of the quadratic function at 'point
   the function is F(x) = 1/2 x' A x + d' x + c
   quadratic: '(:A a-matrix :d a-column-vector :c a-number)
  "
  (let ((A (getf quadratic :A))
        (d (getf quadratic :d))
        (c (getf quadratic :c)))
    (+ (* 1/2 (inner-product point (matrix-product A point)))
       (inner-product d point)
       c)))

(defun shuffle (seq)
  "random sorting for a sequence"
  (alexandria:shuffle seq))

(defun quadratic-function (A &optional d c)
  "quadratic quadratic: F(x) = 1/2 x' A x + d' x + c
   return the function"
  (lambda (x) (reduce #'matrix-add
                      (loop for i in (list (reduce #'matrix-product (list 1/2 (transpose x) A x))
                                           (when d (reduce #'matrix-product (list (transpose d) x)))
                                           (when c c))
                            when i collect i))))

(defun gradient-at-point (ᐁF point)
  "calc the gradient of F at point, ᐁF is a list of n-vars functions, and point is an n-dimensional column vector, the result is a column numerical vector. eg. (gradient-at-point (list #'(lambda (x y) (+ x y)) #'(lambda (x y) (- x y))) '((1) (1)))"
  (transpose (list (loop for f in ᐁF collect (apply f (car (transpose point)))))))

(defgeneric matrix-flatten (m)
  (:documentation "flatten a matrix and convert it to a row vector"))

(defmethod matrix-flatten ((m list))
  "eg. '((1 2) (3 4)) -》 '((1 2 3 4))"
  (list (reduce #'append m)))

(defgeneric data-partition (data part-ratio)
  (:documentation "according to each part's ratio, partition `data into several parts"))

(defmethod data-partition ((data list) (part-ratio list))
  "eg. part-ratio: '(1 1 1), denote three parts and the parts' number is 1:1:1,
note that some part will get nil if iss ratio is too small"
  (let* ((total-ratio (apply #'+ part-ratio))
         (total-data (length data))
         (accumulate-ratio (reverse (mapcar #'(lambda (x) (apply #'+ x))
                                            (mapcon #'list (reverse part-ratio)))))
         (part-interval (loop for i in accumulate-ratio
                              collect (round (* total-data (/ i total-ratio))))))
    (loop for interval>= in (cons 0 part-interval)
          for interval< in part-interval
          collect (subseq data interval>= interval<))))

(defun successive-property-list-p (lst &key test)
  "test if `lst has some successor property"
  (if (cddr lst)
    (and (funcall test (car lst) (cadr lst))
         (successive-property-list-p (cdr lst) :test test))
    (funcall test (car lst) (cadr lst))))

(defun strict-ascending-list-p (lst)
  (successive-property-list-p lst :test #'<))

(defun ascending-list-p (lst)
  (successive-property-list-p lst :test #'<=))

(defun strict-descending-list-p (lst)
  (successive-property-list-p lst :test #'>))

(defun descending-list-p (lst)
  (successive-property-list-p lst :test #'>=))

;;;; some statistic functions
(defun average (lst &optional (sum 0) (num 0))
  (if (null lst)
      (/ sum num)
      (average (cdr lst) (+ sum (car lst)) (+ num 1))))

(defun mean (lst)
  (average lst))

(defun median (lst)
  (let ((sorted (sort lst #'>))
        (n (length lst)))
    (if (= (mod n 2) 0)
        (/ (+ (nth (- (/ n 2) 1) sorted) (nth (/ n 2) lst)) 2)
        (nth (/ (- n 1) 2) lst))))

(defun variance (lst &optional (square-sum 0) (num 0))
  (if (null lst)
      (/ square-sum num)
      (variance (cdr lst)
                (+ square-sum (* (car lst) (car lst)))
                (+ num 1))))

(defun standard-variance (lst)
  (expt (variance lst) 0.5))
