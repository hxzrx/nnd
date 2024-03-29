(in-package #:nnd)

;;;; transpose
(defgeneric transpose (m)
  (:documentation "transposing of a matrix")
  (:method ((m list))
  "transposing a matrix"
    (apply #'mapcar #'list m))
  (:method ((m (eql nil)))
    nil)
  (:method ((m real))
  "transpose(m) = m"
    m))


;;;; basic list +-*/
;;;; DO NOT use them directly!
(defun basic-list-list+ (v1 v2)
  "v1+v2, '(1 2 3) + '(4 5 6) -> '(5 7 9)"
  (assert (listp v1))
  (assert (listp v2))
  (assert (list-length-equal v1 v2))
  (assert (list-check-type v1 'number))
  (assert (list-check-type v2 'number))
  (mapcar #'+ v1 v2))

(defun basic-list-list- (v1 v2)
  "v1-v2 '(1 2 3) + '(4 5 6) -> '(-3 -3 -3)"
  (assert (listp v1))
  (assert (listp v2))
  (assert (list-length-equal v1 v2))
  (assert (list-check-type v1 'number))
  (assert (list-check-type v2 'number))
  (mapcar #'- v1 v2))

(defun basic-list-scalar* (v n)
  "vector multiples a scalar, v is a list and n is a number"
  (assert (and (listp v) (numberp n)))
  (assert (list-check-type v 'number))
  (mapcar #'(lambda (x) (* x n)) v))

(defun basic-list-scalar/ (v n)
  "vector divides a scaler, v is a list and n is a number"
  (assert (and (listp v) (numberp n)))
  (assert (not (= n 0)))
  (mapcar #'(lambda (x) (/ x n)) v))

(defun basic-list-list* (v1 v2)
  "a column vector multiples a row vector, here v1 and v2 are both lists,
   eg: (1 2 3)*(1 2 3) is (transpose (1 2 3)) * (1 2 3), the result is a rank one matrix, ((1 2 3) (2 4 6) (3 6 9))"
  (assert (list-length-equal v1 v2))
  (assert (list-check-type v1 'number))
  (assert (list-check-type v2 'number))
  (loop for i in v1
        collect
        (loop for j in v2
              collect (* i j))))

(defun basic-list-inner-product (vec1 vec2)
  "inner for two lists, eg. '(1 2 3) '(4 5 6)"
  (reduce #'+ (mapcar #'* vec1 vec2)))

(defun basic-list-independent-p (&rest lists)
  "check if the lists are independent"
  (when (= (length lists)
           (matrix-rank lists))
    t))

;;;; all the vectors and matrices are lists of lists, even for a row vector, eg. '((1 2 3))
;;;; addition
(defgeneric row-vector-p (vec)
  (:documentation "check if vec is a row vector"))

(defmethod row-vector-p ((vec list))
  "check if vec is a row vector"
  (and (= (length vec) 1)
       (every #'numberp (car vec))))

(defgeneric column-vector-p (vec)
  (:documentation "check if vec is a column vector"))

(defmethod column-vector-p ((vec list))
  "check if vec is a column vector"
  (every #'(lambda (row) (and (listp row) (= (length row) 1) (numberp (car row)))) vec))

(defmethod column-vector-p ((vec number))
  "return t if vec is a number"
  t)

(defgeneric vector-add (a b)
  (:documentation "additon two vectors, the result is the same as the type of a"))

(defmethod vector-add ((a list) (b list))
  "addition of two vectors of type 'list"
  (if (row-vector-p a)
      (list (basic-list-list+ (car a) (car b)))
      (transpose (vector-add (transpose a) (transpose b)))))


;;;; subtraction
(defgeneric vector-sub (a b)
  (:documentation "subtraction of two vectors"))

(defmethod vector-sub ((a list) (b list))
  "subtraction of two vectors of type 'list"
  (if (row-vector-p a)
      (list (basic-list-list- (car a) (car b)))
      (transpose (vector-sub (transpose a) (transpose b)))))


;;;; vector multiple a scalar
(defgeneric vector-multiply-scalar (v n)
  (:documentation "vector multiply a scalar"))

(defmethod vector-multiply-scalar ((v list) (n number))
  "vector * scalar"
  (if (row-vector-p v)
      (list (basic-list-scalar* (car v) n))
      (transpose (vector-multiply-scalar (transpose v) n))))

(defmethod vector-multiply-scalar ((n number) (v list))
  "scalar * vector"
  (vector-multiply-scalar v n))


;;;; matrix addition
(defgeneric matrix-add (matrix1 matrix2)
  (:documentation "matrix1 + matrix2")
  (:method ((matrix1 list) (matrix2 list))
    "m1+m2"
    (let ((size1 (lists-length-equal matrix1)) ;(rows . cols)
          (size2 (lists-length-equal matrix2)))
      (assert (and size1 size2))
      (assert (and (= (car size1) (car size2))
                   (= (cdr size1) (cdr size2))))
      (if (equal size1 (cons 1 1))
          (+ (caar matrix1) (caar matrix2))
          (loop for m1-row in matrix1
                for m2-row in matrix2
                collect (basic-list-list+ m1-row m2-row)))))
  (:method ((m number) (n number))
    "number + number"
    (+ m n))
  (:method ((matrix list) (n number))
    " '((m)) + n, only for 1 by 1 matrix"
    (assert (equal (matrix-size matrix) (cons 1 1)))
    (+ (caar matrix) n))
  (:method ((n number) (matrix list))
    " n + '((m)), only for 1 by 1 matrix"
    (matrix-add matrix n)))

(defgeneric matrix-dot-add (matrix1 matrix2)
  (:documentation "matrix add element by element")
  (:method ((matrix1 list) (matrix2 list))
    (let ((size1 (matrix-size matrix1)))
      (assert (equal size1 (matrix-size matrix2)))
      (if (equal size1 (cons 1 1))
          (+ (caar matrix1) (caar matrix2))
          (loop for row1 in matrix1
                for row2 in matrix2
                collect
                (loop for elem1 in row1
                      for elem2 in row2
                      collect (+ elem1 elem2))))))
  (:method ((n1 number) (n2 number))
    (+ n1 n2)))

(defun .+ (matrix1 matrix2)
  "matrix dot substract"
  (matrix-dot-add matrix1 matrix2))

;;;; matrix subtraction
(defgeneric matrix-sub (matrix1 matrix2)
  (:documentation "m1 - m2")
  (:method ((matrix1 list) (matrix2 list))
    "m1-m2"
    (let ((size1 (lists-length-equal matrix1))
          (size2 (lists-length-equal matrix2)))
      (assert (and size1 size2))
      (assert (and (= (car size1) (car size2))
                   (= (cdr size1) (cdr size2))))
      (if (equal size1 (cons 1 1))
          (- (caar matrix1) (caar matrix2))
          (loop for i in matrix1
                for j in matrix2
                collect (basic-list-list- i j)))))
  (:method ((matrix1 number) (matrix2 number))
    "mnumber - number"
    (- matrix1 matrix2))
  (:method ((matrix list) (n number))
    "only for 1 by 1 matrix"
    (assert (equal (matrix-size matrix) (cons 1 1)))
    (- (caar matrix) n))
  (:method ((n number) (matrix list))
    (assert (equal (matrix-size matrix) (cons 1 1)))
    (matrix-sub matrix n)))

(defgeneric matrix-dot-sub (matrix1 matrix2)
  (:documentation "matrix substract element by element")
  (:method ((matrix1 list) (matrix2 list))
    (let ((size1 (matrix-size matrix1)))
      (assert (equal size1 (matrix-size matrix2)))
      (if (equal size1 (cons 1 1))
          (- (caar matrix1) (caar matrix2))
          (loop for row1 in matrix1
                for row2 in matrix2
                collect
                (loop for elem1 in row1
                      for elem2 in row2
                      collect (- elem1 elem2))))))
  (:method ((n1 number) (n2 number))
    (- n1 n2)))

(defun .- (matrix1 matrix2)
  "matrix dot substract"
  (matrix-dot-sub matrix1 matrix2))


;;;; matrix multiply a scalar
(defgeneric matrix-multiple-scalar (matrix n)
  (:documentation " matrix * number")
  (:method ((matrix list) (n number))
    "matrix * number"
    (if (equal (matrix-size matrix) (cons 1 1))
        (* (caar matrix) n)
        (loop for row in matrix
              collect
              (basic-list-scalar* row n))))
  (:method ((n number) (matrix list))
    "number * matrix"
    (matrix-multiple-scalar matrix n))
  (:method ((matrix number) (n number))
    "number * number"
    (* matrix n)))

;;;; matrix divide a scalar
(defgeneric matrix-divide-scalar (matrix n)
  (:documentation " matrix / number"))

(defmethod matrix-divide-scalar ((matrix list) (n number))
  "matrix / number"
  (loop for row in matrix
        collect
        (basic-list-scalar/ row n)))

(defmethod matrix-divide-scalar ((matrix number) (n number))
  "number / number"
  (/ matrix n))


;;;; matrix multiply matrix
(defgeneric rank-one-matrix-product (col-vec row-vec)
  (:documentation "n*1 vector multiply 1*n vector"))

(defmethod rank-one-matrix-product ((col-vec list) (row-vec list))
  "n*1 vector multiply 1*n vector, list type, eg. ((1 2 3))' * ((1 2 3))"
  (basic-list-list* (car col-vec) (car row-vec)))

(defgeneric matrix-product (matrix1 matrix2)
  (:documentation "product of two matrix matrix1 and matrix2")
  (:method ((matrix1 list) (matrix2 list))
    "matrix product: m1 * m2"
    (let ((size1 (lists-length-equal matrix1))
          (size2 (lists-length-equal matrix2)))
      (assert (and size1 size2))
      (assert (= (cdr size1) (car size2)))
      (if (and (= (car size1) 1) (= (cdr size2) 1))
          (inner-product matrix1 (transpose matrix2))
          (loop for row in matrix1
                collect (loop for col in (transpose matrix2)
                              collect (basic-list-inner-product row col))))))
  (:method ((matrix list) (n number))
    "matrix * n"
    (matrix-multiple-scalar matrix n))
  (:method ((n number) (matrix list))
    "n * matrix"
    (matrix-multiple-scalar matrix n))
  (:method ((matrix list) (zero (eql 0)))
    "matrix * 0"
    (loop for row in matrix
          collect (loop for element in row
                        collect 0)))
  (:method ((m number) (n number))
    "m * n"
    (* m n)))

(defun matrix-multi-product (&rest matrices)
  " m1 * m2 * ... * mn"
  (reduce #'matrix-product (remove nil matrices)))

(defun matrix-multi-add (matrices)
  "m1 + m2 + ... + mn, if the input is nil, this function will return nil"
  (alexandria:when-let (matrices-removed-nil (remove nil matrices))
    (reduce #'matrix-add matrices-removed-nil)))

(defgeneric matrix-dot-product (matrix1 matrix2)
  (:documentation "matrix product element by element")
  (:method ((matrix1 list) (matrix2 list))
    (let ((size1 (matrix-size matrix1)))
      (assert (equal size1 (matrix-size matrix2)))
      (if (equal size1 (cons 1 1))
          (* (caar matrix1) (caar matrix2))
          (loop for row1 in matrix1
                for row2 in matrix2
                collect
                (loop for elem1 in row1
                      for elem2 in row2
                      collect (* elem1 elem2))))))
  (:method ((n1 number) (n2 number))
    (* n1 n2)))

(defun .* (matrix1 matrix2)
  "matrix dot substract"
  (matrix-dot-add matrix1 matrix2))

;;;; Element-wise op

(defgeneric element-wise (matrix1 matrix2 op)
  (:documentation "element-wise operation between matrix1 and matrix2")
  (:method ((matrix1 list) (matrix2 list) (op function))
    (assert (equal (matrix-size matrix1) (matrix-size matrix2)))
    (loop for row1 in matrix1
          for row2 in matrix2
          collect
          (loop for elem1 in row1
                for elem2 in row2
                collect (funcall op elem1 elem2))))
  (:method ((n1 number) (n2 number) (op function))
    (funcall op n1 n2)))

(defun element-wise-add (matrix1 matrix2)
  "the same as matrix-dot-add"
  (element-wise matrix1 matrix2 #'+))

(defun element-wise-sub (matrix1 matrix2)
  (element-wise matrix1 matrix2 #'-))

(defun element-wise-mul (matrix1 matrix2)
  (element-wise matrix1 matrix2 #'*))

(defun element-wise-div (matrix1 matrix2)
  (element-wise matrix1 matrix2 #'/))

(defun element-wise-exp (matrix1 matrix2)
  (element-wise matrix1 matrix2 #'expt))

(defun element-wise-self (matrix op)
  (element-wise matrix matrix op))

;;;; inner product
(defgeneric inner-product (a b)
  (:documentation "inner product of vector a and b"))

(defmethod inner-product ((a list) (b list))
  "inner product, row vetors and column vectors are allowable, row vector is only for simplicity.
   '((1 2 3)) dot '((1 2 3)), or '((1) (2) (3)) dot '((1) (2) (3)), the results are the same"
  ;;test (inner-product '((1 2 3)) '((1 2 3)))
  ;;test (inner-product '((1) (2) (3)) '((1) (2) (3)))
  (assert (list-length-equal (car a) (car b)))
  ;(assert (list-check-type (car a) 'number))
  ;;(assert (list-check-type (car b) 'number))
  (if (null (cdar a)) ;; if column vector
      (basic-list-inner-product (car (transpose a)) (car (transpose b)))
      (basic-list-inner-product (car a) (car b)))) ;row vector

(defmethod inner-product ((a number) (b number))
  "for special case, returns a*b"
  (* a b))

(defgeneric inner-product-self (a)
  (:documentation "vetor's inner product with itself, (a a)"))

(defmethod inner-product-self ((a list))
  "inner product (a a)"
  (inner-product a a))

(defmethod inner-product-self ((a number))
  "special case, inner product (a a)"
    (* a a))

;;;; matrix size
(defgeneric matrix-size (m)
  (:documentation "return the size of a matrix or vector, (rows . cols) , for a row vector, return (1 . cols)"))

(defmethod matrix-size ((m list))
  "Return a cons (rows . cols). If m is not rectangle, return nil."
  (lists-length-equal m))

(defmethod matrix-size ((m number))
  "Return a cons (rows . cols). If m is not rectangle, return nil."
  (cons 1 1))

(defgeneric matrix-elements-num (matrix)
  (:documentation "how elements in the matrix")
  (:method ((matrix list))
    (let ((size (matrix-size matrix)))
      (* (car size) (cdr size))))
  (:method ((matrix number))
    1))

;;;; ith row of a matrix
(defgeneric nth-row (matrix nth)
  (:documentation "return the nth row of matrix, return a list, '((1 2 3))"))

(defmethod nth-row ((matrix list) (n-th integer))
  "return the nth row of matrix"
  (assert (> (car (matrix-size matrix)) n-th))
  (list (nth n-th matrix)))

;;;; nth column of a matrix
(defgeneric nth-col (matrix nth)
  (:documentation "return the nth column of matrix, return a row vector, '((1) (2) (3)))"))

(defmethod nth-col ((matrix list) (n-th integer))
  "return the nth col of matrix"
  (assert (> (cdr (matrix-size matrix)) n-th))
  (loop for row in matrix
        collect (list (nth n-th row))))


;;;; row exchange of a matrix
(defgeneric row-exchange (matrix i j)
  (:documentation "exchange the ith and jth row of matrix"))

(defmethod row-exchange ((matrix list) (i integer) (j integer))
  "row exchanged"
  (if (= i j) matrix
      (progn
        (let ((rows (car (matrix-size matrix))))
          (assert (and (> rows i) (> rows j))))
        (let ((min-row (nth (min i j) matrix))
              (max-row (nth (max i j) matrix)))
          (loop for r   in   matrix
                for idx from 0
                collect (cond ((= idx (min i j)) max-row)
                              ((= idx (max i j)) min-row)
                              (t r)))))))

;;;; column exchange of a matrix
(defgeneric col-exchange (matrix i j)
  (:documentation "exchange the ith and jth column of matrix"))

(defmethod col-exchange ((matrix list) (i integer) (j integer))
  "column exchanged"
  (if (= i j)
      matrix
      (progn
        (let ((cols (cdr (matrix-size matrix))))
          (assert (and (> cols i) (> cols j))))
        (loop for each-row in matrix
              collect (loop for each-elem in each-row
                            for idx from 0
                            collect
                            (let ((min-elem (nth (min i j) each-row))
                                  (max-elem (nth (max i j) each-row)))
                              (cond ((= idx (min i j)) max-elem)
                                    ((= idx (max i j)) min-elem)
                                    (t each-elem))))))))

;;;; exchange the first row with the row that has the greatest sbsolute value of the element on the specified column
(defgeneric row-exchange-with-greatest-pivot (matrix nth-pivot)
  (:documentation "exchange the current pivot row with the row that has the greatest sbsolute value of the element under the pivot place"))

(defmethod row-exchange-with-greatest-pivot (matrix nth-pivot)
  "exchange the current pivot row with the row that has the greatest sbsolute value of the element under the pivot place"
    (row-exchange matrix nth-pivot (posioning-greatest-pivot matrix nth-pivot)))

;;;; row elimination, up down, and get an upper triangle matrix
(defgeneric row-elimination-up-down (matrix)
  (:documentation "row elimination, the result is an upper triangle"))

(defun row-elimination-up-down% (matrix nth-pivot)
  "row elimination, elimination for a column!"
  (let* ((pivot-greatest-matrix (row-exchange-with-greatest-pivot matrix nth-pivot))
         (pivot-row (nth nth-pivot pivot-greatest-matrix))
         (pivot-value (nth nth-pivot pivot-row)))
    ;;(format t "matrix: ~d~&pivot-greatest-matrix: ~d~&pivot-row: ~d~&pivot-value: ~d~%" matrix pivot-greatest-matrix pivot-row pivot-value)
    (loop for row in   pivot-greatest-matrix
          for i   from 0
          collect (cond ((<= i nth-pivot) row)
                        ((= pivot-value 0) row)
                        ((= (nth nth-pivot row) 0) row)
                        (t (basic-list-list- row (basic-list-scalar* pivot-row (* (/ 1 pivot-value) (nth nth-pivot row)))))))))

(defun row-elimination-up-down%% (matrix square &optional (elim-idx 0))
  "row elimination, elimination for the first row to the square'th row"
  (if (= elim-idx square)
      matrix
      (row-elimination-up-down%% (row-elimination-up-down% matrix elim-idx) square (incf elim-idx))))

(defmethod row-elimination-up-down ((matrix list))
  "row elimination, for all columns! the result is an upper triangle"
  (let ((size (matrix-size matrix)))
    (row-elimination-up-down%% matrix (min (car size) (cdr size)))))


;;;; row elimination, down to up, the input matrix is an upper triangle matrix, and the result is an diag matrix
(defgeneric row-elimination-down-up (matrix)
  (:documentation "row elimination, the resule is an diag triangle matrix,
                   this should be restricted that the matrix is already an upper triangle"))

(defun row-elimination-down-up% (matrix pivot-id)
  "for the special case that row-num <= column-num"
  ;;(print-matrix matrix)
  (if (= pivot-id -1)
      matrix
      (row-elimination-down-up%
       (let* ((pivot-row (nth pivot-id matrix))
              (pivot-val (nth pivot-id pivot-row))
              (row-num (length matrix)))
         ;;(format t "~&pivot id: ~d, row: ~d, val: ~d~%" pivot-id pivot-row pivot-val)
         (loop for r from 0 below row-num
               collect (progn
                         (if (>= r pivot-id)
                             (nth r matrix)
                             (if (= pivot-val 0)
                                 pivot-row
                                 (basic-list-list- (nth r matrix)
                                                   (basic-list-scalar* pivot-row
                                                                       (/ (nth pivot-id (nth r matrix)) pivot-val))))))))
         (1- pivot-id))))

(defmethod row-elimination-down-up ((matrix list))
  "row elimination, down to top, this step will get an lower triangle.
   this will only affect on the max top-left square submatrix"
  (let ((size (matrix-size matrix)))
    (if (<= (car size) (cdr size)) ; row <= col
        (row-elimination-down-up% matrix (1- (car size)))
        (append (row-elimination-down-up% (loop for row in matrix
                                                for i below (cdr size)
                                                collect row)
                                          (1- (cdr size)))
                (nthcdr (cdr size) matrix)))))

(defgeneric matrix-divided-by-pivot (matrix)
  (:documentation "after the elimination from down to top, we should make the pivots ones"))

(defmethod matrix-divided-by-pivot ((matrix list))
  "divide each row by its pivot.
   when the matrix is a diagonal, the result will be an identity matrix"
  (let* ((size (matrix-size matrix)))
    (loop for row in matrix
          for i from 0
          collect (progn (if (< i (min (car size) (cdr size)))
                             (cond ((= (nth i row) 0) row)
                                   (t (basic-list-scalar* row (/ 1 (nth i row)))))
                             row)))))

;;;; make an augmented matrix
(defgeneric make-augmented (matrix1 matrix2)
  (:documentation "make the augmented matrix with matrix1 and matrix2,
                   if A = [v1 v2 ... vm], B= [w1 w2 ... vn], the augmented matrix is [v1 v2 ... vm w1 w2 ... wn]"))

(defmethod make-augmented ((matrix1 list)  (matrix2 list))
  "make the augmented matrix of matrix and matrix2"
  (assert (= (car (matrix-size matrix1)) (car (matrix-size matrix2))))
  (loop for row1 in matrix1
        for row2 in matrix2
        collect (append row1 row2)))

(defmethod make-augmented ((n1 number) (n2 number))
  (list (list n1 n2)))

(defmethod make-augmented ((n1 number) (matrix2 list))
  (assert (= (car (matrix-size matrix2)) 1))
  (list (cons n1 (car matrix2))))

(defmethod make-augmented ((matrix1 list) (n2 number))
  (assert (= (car (matrix-size matrix1)) 1))
  (list (append (car matrix1) (list n2))))


(defgeneric matrix-inverse (matrix)
  (:documentation "calc the reverse of matrix"))

(defmethod matrix-inverse ((matrix list))
  "calc the reverse of matrix"
  (let ((size (matrix-size matrix)))
    (assert (= (car size) (cdr size)))
    (let* ((augmented (make-augmented matrix (eye (car size))))
           (upper-triangle (row-elimination-up-down augmented))
           (diag-matrix (row-elimination-down-up upper-triangle))
           (left-eye (matrix-divided-by-pivot diag-matrix)))
      (if (eye-p (matrix-left-columns left-eye (car size)))
          (matrix-right-columns left-eye (car size))
          nil))))

(defgeneric matrix-pseudoinverse (matrix)
  (:documentation "Moore-Penrose pseudoinverse,
                   when N(rows) > N(columns), and the columns are independent, P+ = inverse(P'P) P'
                   when N(rows) < N(columns), and the rows are independent, P+ = P' inverse(PP')"))

(defmethod matrix-pseudoinverse ((matrix list))
  "Moore-Penrose pseudoinverse"
  (let ((size (matrix-size matrix)))
    (cond ((and (> (car size) (cdr size)) (matrix-column-independent-p matrix))
           (matrix-product (matrix-inverse (matrix-product (transpose matrix) matrix))
                           (transpose matrix)))
          ((and (< (car size) (cdr size)) (matrix-row-independent-p matrix))
           (matrix-product (transpose matrix)
                           (matrix-inverse (matrix-product matrix (transpose matrix)))))
          ((matrix-row-independent-p matrix) (matrix-inverse matrix))
          (t (format t "This matrix has no pseudoinverse.")
             (print-matrix matrix)
             (error "This matrix has no pseudoinverse.")))))






;;;; Gauss-Jordan Elimination
(defgeneric gauss-jordan-elimination (matrix)
  (:documentation "Calculating 1/A by Gauss-Jordan Elimination"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; construct a special matrix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; construct a matrix with all zero elements
(defgeneric make-zeros (m &optional n)
  (:documentation "return a matrix which elements are all 0's. If n is nil, return a column vector such as '((0) (0) (0))")
  (:method ((m integer) &optional (n 1))
    "return a full zero matrix"
    (loop for row below m
          collect (loop for col below n
                        collect 0)))
  (:method ((m (eql 1)) &optional (n 1))
    (if (eq n 1) 0 (loop for row below m
                         collect (loop for col below n
                                       collect 0)))))

(defgeneric make-zero-vector (dimension)
  (:documentation "return a vector with all elements are 0")
  (:method ((dimension integer))
    (make-zeros dimension)))

(defgeneric make-zeros-from-template (template-matrix)
  (:method ((template-matrix list))
    (let* ((rank (matrix-size template-matrix))
           (rows (car rank))
           (cols (cdr rank)))
      (make-zeros rows cols)))
  (:method ((template-matrix number))
    0))

(defgeneric matrix-zeros-p (matrix)
  (:documentation "check if the matrix's elemets are all zeros")
  (:method ((matrix list))
    "check if the matrix's elemets are all zeros"
    (every #' list-zeros-p matrix))
  (:method ((n number))
    (= n 0)))

(defgeneric make-ones (m &optional n)
  (:documentation "return a matrix which elements are all 1's. If n is nil, return a row vector such as '((1 1 1))")
  (:method ((m integer) &optional (n 1))
    "return a full zero matrix"
    (loop for row below m
          collect (loop for col below n
                        collect 1)))
  (:method ((m (eql 1)) &optional (n 1))
    (if (eq n 1) 1 (loop for row below m
                         collect (loop for col below n
                                       collect 1)))))

(defgeneric make-ones-from-template (template-matrix)
  (:method ((template-matrix list))
    (let* ((rank (matrix-size template-matrix))
           (rows (car rank))
           (cols (cdr rank)))
      (make-ones rows cols)))
  (:method ((template-matrix number))
    1))


(defgeneric make-rows (&rest rows)
  (:documentation "make a row vector with the provided parameters"))

(defmethod make-rows (&rest rows)
  "make a row vector with the provided parameters"
  (list (loop for i in rows collect i)))

(defgeneric make-cols (&rest cols)
  (:documentation "make a col vector with the provided parameters"))

(defmethod make-cols (&rest cols)
  "make a col vector with the provided parameters"
  (loop for i in cols collect (list i)))

(defmethod from-list ((lst list) (rows integer) (cols integer))
  "make a `rows' * `cols' matrix whose elements are in `lst'"
  (if (and (= rows 1) (= cols 1))
      (first lst)
      (loop for i from 0 below rows
            collect (loop for j from 0 below cols
                          append (list (pop lst))))))

(defgeneric eye (n)
  (:documentation "make an identity matrix with rank n"))

(defmethod eye ((n integer))
  "identity matrix"
  (loop for row below n
        collect (loop for col below n
                      collect (if (= row col) 1 0))))

(defmethod eye-p% (matrix idx &optional (epsilon 0.001))
  "for real matrix, when epsilon is too small, the result will not stable"
  (if (null matrix) t
      (and (list-given-place-1-others-0 (car matrix) idx 0 epsilon)
           (eye-p% (cdr matrix) (1+ idx) epsilon))))

(defgeneric eye-p (matrix)
  (:documentation "check if m is an identity matrix"))

(defmethod eye-p ((matrix list))
  "check if m is an identity matrix"
  (let ((size (matrix-size matrix)))
    (assert (= (car size) (cdr size)))
    (eye-p% matrix 0)))


(defgeneric diag (&rest diags)
  (:documentation "make a diagonal matrix"))

(defmethod diag (&rest diags)
  "make a diag matrix, parameters are the diags"
  (let ((n (length diags)))
    (if (= n 1)
        (car diags)
        (loop for row below n
              for diag in diags
              collect (loop for col below n
                            collect (if (= row col) diag 0))))))

(defun diag-from-list (diag-list)
  "make a diag matrix from a list"
  (if (numberp diag-list)
      diag-list
      (let ((n (length diag-list)))
        (loop for row below n
              for diag in diag-list
              collect (loop for col below n
                            collect (if (= row col) diag 0))))))

(defgeneric get-matrix-diag (matrix)
  (:documentation "collect the diagonal element and return a list")
  (:method ((matrix list))
    (let ((rank (matrix-size matrix)))
      (loop for rows in matrix
            for i from 0 below (min (car rank) (cdr rank))
            collect (nth i rows))))
  (:method ((n number))
    n))

(defun make-single (len single-place)
  "make a column vector with 1 in the specified placed and zeros else where"
  (assert (<= single-place (1- len)))
  (assert (and (> len 0) (>= single-place 0)))
  (if (and (= len 1) (= single-place 0)) 1
      (transpose (list (loop for i from 0 below len
                             collect (if (= i single-place) 1 0))))))

;;;; make a m by n random matrix
(defgeneric rand-matrix (m n &optional min max)
  (:documentation "make a rand matrix with m rows and n columns, each element between min and max, if min or max was not provided, the elements were real random numbers between 0 and 1"))

(defmethod rand-matrix ((m integer) (n integer) &optional min max)
  (cond ((and (null min) (null max))
         (loop for row from 0 below m
               collect (loop for col from 0 below n
                             collect (random 1.0))))
        ((or (and min (null max)) (and max (null min)))
         (loop for row from 0 below m
               collect (loop for col from 0 below n
                             collect (* (signum (or min max))
                                        (random (abs (or min max)))))))
        ((and min max)
         (assert (>= max min))
         (if (> max min)
             (loop for row from 0 below m
                   collect (loop for col from 0 below n
                                 collect (+ min (random (- max min)))))
             (loop for row from 0 below m
                   collect (loop for col from 0 below n
                                 collect min))))
        (t (error "max < min"))))

(defmethod rand-matrix ((m (eql 1)) (n (eql 1)) &optional min max)
  "special case, 1 * 1 matrix is a number"
  (cond ((and (null min) (null max)) (random 1.0))
        ((or (and min (null max)) (and max (null min)))
         (* (signum (or min max))
            (random (abs (or min max)))))
        ((and min max)
         (assert (>= max min))
         (if (> max min) (+ min (random (- max min))) min))
        (t (error "max < min"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; determinant
(defgeneric make-submatrix (matrix i j)
  (:documentation "make a new matrix from the first parameter, but abandon its ith row and jth col"))

(defmethod make-submatrix ((matrix list) (i integer) (j integer))
  "make a new matrix from the first parameter, but abandon its ith row and jth col"
  (loop for row in matrix
        for row-idx from 0
        when (not (= row-idx i))
          collect (loop for col in row
                        for col-idx from 0
                        when (not (= col-idx j))
                          collect col)))

(defgeneric matrix-left-columns (matrix k)
  (:documentation "the submatrix constructed of the left k columns of matrix"))

(defmethod matrix-left-columns ((matrix list) (k integer))
  "for a m by n matrix, return the submatrix constructed of the left k columns of the matrix, where 1<= k <= n"
  (assert (and (> k 0) (<= k (length (car matrix)))))
  (loop for row in matrix
        collect
        (loop for col   in   row
              for c-idx from 0   below k
              collect col)))

(defgeneric matrix-right-columns (matrix k)
  (:documentation "the submatrix constructed of the right k columns of matrix"))

(defmethod matrix-right-columns ((matrix list) (k integer))
  "for a m by n matrix, return the submatrix constructed of the left k columns of the matrix, where 1<= k <= n"
  (let ((len (length (car matrix))))
    (assert (and (> k 0) (<= k len)))
    (loop for row in matrix
          collect
          (loop for col   in   row
                for c-idx from 0
                when (>= c-idx (- len k))
                  collect col))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; determinant
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric det% (matrix size)
  (:documentation "return the determinant of a square matrix"))

(defmethod det% ((matrix list) (size integer))
  "return the determinant of a square matrix"
  (cond ((= size 1) (caar matrix))
        ((= size 2) (let ((first-row  (first  matrix))
                          (second-row (second matrix)))
                      (- (* (first  first-row) (second second-row))
                         (* (second first-row) (first  second-row)))))
        (t (reduce #'+ (loop for i from 0 below size
                             for col-element in (first matrix)
                             collect (* (if (= (mod (+ 0 i) 2) 0) 1 -1)
                                        col-element
                                        (det% (make-submatrix matrix 0 i) (1- size))))))))

(defgeneric det (matrix)
  (:documentation "return the determinant of a square matrix"))

(defmethod det ((matrix list))
  (let ((size (matrix-square-p matrix)))
    (assert size)
    (det% matrix size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric matrix-trace (matrix)
  (:documentation "Trace (sum of diagonal elements) of matrix"))

(defmethod matrix-trace ((matrix list))
  "trace of matrix"
  (apply #'+ (loop for row    in   matrix
                   for row-id from 0
                   collect (nth row-id row))))

;;;; singular
(defgeneric singular-p (square)
  (:documentation "check if a square matrix is singular, if it's singular, return t, else nil"))

(defmethod singular-p ((square list))
  "check if a square matrix is singular, if it's singular, return t, else nil"
  (assert (matrix-square-p square))
  (when (= (det square) 0) t))

(defgeneric matrix-rank (matrix)
  (:documentation "return the rank of the matrix"))

(defmethod matrix-rank% (matrix &optional (rank 0))
  (cond ((null matrix) rank)
        ((list-zeros-p (car matrix)) rank)
        (t (matrix-rank% (cdr matrix) (1+ rank)))))

(defmethod matrix-rank ((matrix list))
  "return the rank of the matrix"
  (let ((lower-triangle (row-elimination-up-down matrix)))
    (matrix-rank% lower-triangle)))

(defgeneric matrix-row-independent-p (matrix)
  (:documentation "check if the rows of matrix are independent"))

(defmethod matrix-row-independent-p ((matrix list))
  "check if the rows of matrix are independent"
  (if (= (length matrix) (matrix-rank matrix)) t nil))

(defgeneric matrix-column-independent-p (matrix)
  (:documentation "check if the columns of matrix are independent"))

(defmethod matrix-column-independent-p ((matrix list))
  "check if the columns of matrix are independent"
  (let ((matrixT (transpose matrix)))
    (if (= (length matrixT) (matrix-rank matrixT)) t nil)))


;;;; solve linear equations

(defgeneric solve-unique (square b)
  (:documentation "solve Ax=b, only for n unkown and n equations. if unsolvable, return nil, else return a row vector"))

(defmethod solve-unique ((square list) (b list))
  "Solve Ax=b, using Crame's Rule"
  (let ((m-size (matrix-square-p square))
        (v-size (cdr (lists-length-equal square))))
    (assert m-size)
    (assert (= m-size v-size))
    (let ((detA (det square)))
      (if (> detA 0)
          (loop for i from 0 below m-size
                collect (/ (det (replace-col square b i)) detA))
          nil))))


;;;; norm
(defgeneric norm (vec)
  (:documentation "norm or a vector"))

(defmethod norm ((vec list))
  "norm or a column vector, vec is allowable for column vectors and row vectors"
  (sqrt (inner-product vec vec)))

(defgeneric vector-length (vector)
  (:documentation "calc Euclid length of a vector, the input vector should be a column vector")
  (:method ((vector list))
    (sqrt (inner-product vector vector)))
  (:method ((vector number))
    (abs vector)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; normalize a vetor

(defgeneric normalize-vector (vec)
  (:documentation "normalize a vec so that it has length 1")
  (:method ((vec list))
    "normalize a vetor, both row vector and column vector are allowed"
    (let ((norm^2 (inner-product vec vec))
          (epsilon 0.00000001))
      (if (< norm^2 epsilon)
          (progn
            (let ((size (matrix-size vec)))
              (make-zeros (car size) (cdr size))))
          (matrix-multiple-scalar vec (/ 1 (sqrt norm^2)))))))

(defgeneric normalize (vector &optional normalized-len)
  (:documentation "normalize a vector so that it's length is `len'")
  (:method ((vector list) &optional (normalized-len 1))
    (matrix-product normalized-len (normalize-vector vector))))

(defgeneric normalize-matrix (matrix &optional normalized-len)
  (:documentation "normalize a matrix so that its row has the same length")
  (:method ((matrix list) &optional (normalized-len 1))
    (loop for row in matrix
          append (normalize (list row) normalized-len))))

;;;; orthogonalization
(defgeneric orthogonalization (matrix)
  (:documentation
   "Gram-Schmidt Orthogonalization.
    For n independent vector y1, ... , yn, transfer them to n orthogonalized vetors v1, ... , vn
    v1 = y1
    vk = yk - \sum_{i=1}^{k-1}(vi yk) /(vi vi) vi
    for simplicity, the input vectors are columns of matrix, and return a matrix whose column vectors are orthogonalized
  "))

(defmethod orthogonalization ((matrix list))
  "Gram-Schmidt Orthogonalization"
  ;; test (orthogonalization (eye 3))
  (let ((to-rows (transpose matrix)) ;col to row so that they are suitable to be processed as lists
        (orthed-rows nil))
    (loop for row in to-rows
          for k   from 0
          do (progn (if (= k 0)
                        (push row orthed-rows)
                        (push (car (vector-sub
                                    (nth-row matrix k)
                                    (reduce #'vector-add
                                            (loop for i from 0 to (1- k)
                                                  collect
                                                  (vector-multiply-scalar
                                                   (nth-row orthed-rows i)
                                                   (/ (inner-product (nth-row orthed-rows i)
                                                                     (nth-row matrix      k))
                                                      (inner-product (nth-row orthed-rows i)
                                                                     (nth-row orthed-rows i))))))))
                              orthed-rows))))
    (transpose (reverse orthed-rows))))


;;;; projection
(defgeneric projection (b a)
  (:documentation "the projection of b onto the line through a, the result vevtor p = (a b)/(a a) * a"))

(defmethod projection ((b list) (a list))
  "the projection of b onto the line through a, the result is an column vetor"
  (assert (eql (row-vector-p b) (row-vector-p a)))
  (vector-multiply-scalar a (/ (inner-product b a) (inner-product a a))))

(defgeneric eigenvalues (matrix)
  (:documentation "return a list of eigenvalues of matrix"))

(defmethod eigenvalues ((matrix list))
  "return a list of eigenvalues of matrix, currently only for 2 by 2 matrix"
  ;; test (eigenvalues '((10 -6) (-6 10))) ==> '(16 4)
  (cond ((equal (matrix-size matrix) '(2 . 2))
         (let ((a (first  (first  matrix)))
               (b (second (first  matrix)))
               (c (first  (second matrix)))
               (d (second (second matrix))))
           (list (/ (+ (+ a d) (sqrt (- (* (+ a d) (+ a d))
                                    (* 4 (- (* a d) (* b c))))))
                    2)
                 (/ (- (+ a d) (sqrt (- (* (+ a d) (+ a d))
                                        (* 4 (- (* a d) (* b c))))))
                    2))))
        (t (format t "~&Not implemented.~%"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;Directional Derivatives
(defgeneric directional-derivative (direction gradient)
  (:documentation "the derivative of the function along the direction. the result is (p Gradient) / norm(p)"))

(defmethod directional-derivative ((direction list) (gradient list))
  "directional derivative"
  (/ (inner-product direction gradient) (norm direction)))

(defgeneric directional-derivative-2 (direction gradient)
  (:documentation "the second order derivative of the function along the direction. the result is (p Gradient) / norm(p)"))

(defmethod directional-derivative-2 ((direction list) (hessian list))
  "second order directional derivative,
   direction should be an column vector, and hessian is the Hessian matrix"
  (matrix-product (matrix-product (transpose direction) hessian) direction))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric sum-squares (matrix)
  (:documentation "sum of the squares of the elements"))

(defmethod sum-squares ((matrix list))
  "sum of the squares of the elements"
  (loop for row in matrix
        sum (loop for element in row
                  sum(* element element))))

;;;; create matrices from template
(defun make-matrix-from-template (t-matrix lst)
  "make a matrix, which elements will be the ones in `lst and has the same size as `t-matrix"
  (if (listp t-matrix)
      (loop for row in t-matrix
            collect (loop for col in row
                          collect (pop lst)))
      (pop lst))) ; numberp

(defgeneric make-random-matrix-from-template (template-matrix &optional min max)
  (:documentation "make a matrix that has the same rank as template-matrix, and its elements will be random.
if min and max were nil, the elements will be real number in [0, 1]"))

(defmethod make-random-matrix-from-template ((template-matrix list) &optional min max)
  (let* ((rank (matrix-size template-matrix))
         (row-num (car rank))
         (col-num (cdr rank)))
    (rand-matrix row-num col-num min max)))

(defmethod make-random-matrix-from-template ((template-matrix number) &optional min max)
  "return a random number"
  (rand-matrix 1 1 min max))

(defgeneric matrix-compare (m1 m2 &key test)
  (:documentation "compare two matrix element by element by `test function"))

(defmethod matrix-compare ((m1 real) (m2 real) &key test)
  "compare two real number by `test function"
  (funcall test m1 m2))

(defun list-compare% (lst1 lst2 &key test)
  "compare two numerical list by `test function"
  (every test lst1 lst2))

(defmethod matrix-compare ((m1 list) (m2 list) &key test)
  "compare two matrix element by element by `test function"
  (assert (equal (matrix-size m1) (matrix-size m2)))
  (every #'(lambda (l1 l2) (list-compare% l1 l2 :test test)) m1 m2))

(defgeneric matrix-slice% (matrix num)
  (:documentation "slice `matrix uniformly into `num parts, the results will include zeros matrix and `matrix itselt"))

(defmethod matrix-slice% ((matrix list) (num integer))
  "slice `matrix uniformly into `num parts, the results will include zeros matrix and `matrix itselt"
  (assert (> num 0))
  (if (> num 1)
      (let ((step (matrix-divide-scalar matrix (1- num))))
        (loop for i from 0 to (1- num)
              collect (matrix-multiple-scalar step i)))
      matrix))

(defmethod matrix-slice% ((matrix number) (num integer))
  "slice a number uniformly into `num parts, the results will include 0 and the number itselt"
  (assert (> num 0))
  (if (> num 1)
      (let ((step (/ matrix (1- num))))
        (loop for i from 0 to (1- num)
              collect (* step i)))
      matrix))

(defgeneric matrix-slice (matrix1 matrix2 num)
  (:documentation "slice matrices uniformly from matrix1 to matrix2 into `num parts, the results will include matrix1 and matrix2"))

(defmethod matrix-slice ((matrix1 list) (matrix2 list) (num integer))
  "slice matrices uniformly from matrix1 to matrix2 into `num parts, the results will include matrix1 and matrix2"
  (assert (> num 0))
  (if (> num 1)
      (loop for slice in (matrix-slice% (matrix-sub matrix2 matrix1) num)
            collect (matrix-add matrix1 slice))
      (matrix-divide-scalar (matrix-add matrix1 matrix2) 2)))

(defmethod matrix-slice ((n1 number) (n2 number) (num integer))
  "slice numbers uniformly from n1 to n22 into `num parts, the results will include n1 and n2"
  (assert (> num 0))
  (if (> num 1)
      (loop for slice in (matrix-slice% (matrix-sub n2 n1) num)
            collect (matrix-add n1 slice))
      (matrix-divide-scalar (matrix-add n1 n2) 2)))

(defgeneric matrix-translation (matrix translate-scale)
  (:documentation "matrix translating"))

(defmethod matrix-translation ((matrix list) (translate-scale number))
  "matrix add another matrix whose elements are the same"
  (let ((size (matrix-size matrix)))
    (matrix-add matrix (matrix-multiple-scalar (make-ones (car size) (cdr size)) translate-scale))))

(defmethod matrix-translation ((translate-scale number) (matrix list))
  "matrix add another matrix whose elements are the same"
  (let ((size (matrix-size matrix)))
    (matrix-add matrix (matrix-multiple-scalar (make-ones (car size) (cdr size)) translate-scale))))

(defmethod matrix-translation ((translate-scale number) (matrix number))
  "number + number"
  (+ matrix translate-scale))


(defgeneric kroncker-product (A B)
  (:documentation "A ⊗ B, Kronecker product of A and B")
  (:method ((A list) (B list))
    "A ⊗ B"
    (loop for A-row in A
          append
          (reduce #'make-augmented
                  (loop for element in A-row
                        collect (matrix-product element B)))))
  (:method ((a number) (B list))
    "number ⊗ matrix"
    (matrix-product a B))
  (:method ((A list) (b number))
    "matrix ⊗ number"
    (matrix-product A b))
  (:method ((a number) (b number))
    "number ⊗ number"
    (* a b)))

(defgeneric vec-operator (matrix)
  (:documentation "transforms a matrix into a vector by stacking the columns of the matrix one underneath the other")
  (:method ((matrix number)) matrix)
  (:method ((matrix list))
    (mapcar #'list (apply #'append (transpose matrix)))))

(defun matrix-to-vector (matrix)
  (vec-operator matrix))

(defgeneric distance (vector1 vector2 &optional type)
  (:documentation "distance of two vector, type :e means Euclidean distance, :m means Manhattan distance, :c means Chebyshev distance")
  (:method ((vector1 list) (vector2 list) &optional (type :e))
    (if (column-vector-p vector1)
        (ecase type
          (:e (sqrt (loop for x in vector1
                          for y in vector2
                          sum (* (- (car x) (car y)) (- (car x) (car y))))))
          (:m (loop for x in vector1
                    for y in vector2
                    sum (abs (- (car x) (car y)))))
          (:c (apply #'max (loop for x in vector1
                                 for y in vector2
                                 collect (abs (- (car x) (car y)))))))
        (distance (transpose vector1) (transpose vector2))))
  (:method ((vector1 number) (vector2 number) &optional type)
    (declare (ignore type))
    (abs (- vector1 vector2))))
