(in-package #:nnd)

;;;; transpose
(defgeneric transpose (m)
  (:documentation "transposing of a matrix"))

(defmethod transpose ((m list))
  "transposing a matrix"
  (apply #'mapcar #'list m))


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
        
;;;; all the vectors and matrices are lists of lists, even for a row vector, eg. '((1 2 3))
;;;; addition
(defgeneric vector-add (a b)
  (:documentation "additon two vectors"))

(defmethod vector-add ((a list) (b list))
  "addition of two vectors of type 'list"
  (list (basic-list-list+ (car a) (car b))))


;;;; subtraction
(defgeneric vector-sub (a b)
  (:documentation "subtraction of two vectors"))

(defmethod vector-sub ((a list) (b list))
  "subtraction of two vectors of type 'list"
  (list (basic-list-list- (car a) (car b))))


;;;; vector multiple a scalar
(defgeneric vector-multiply-scalar (v n)
  (:documentation "vector multiply a scalar"))

(defmethod vector-multiply-scalar ((v list) (n number))
  "vector * scalar"
  (list (basic-list-scalar* (car v) n)))

(defmethod vector-multiply-scalar ((n number) (v list))
  "scalar * vector"
  (list (basic-list-scalar* (car v) n)))


;;;; matrix addition
(defgeneric matrix-add (matrix1 matrix2)
  (:documentation "matrix1 + matrix2"))

(defmethod matrix-add ((matrix1 list) (matrix2 list))
  "m1+m2"
  (let ((size1 (lists-length-equal matrix1)) ;(rows . cols)
        (size2 (lists-length-equal matrix2)))
    (assert (and size1 size2))
    (assert (and (= (car size1) (car size2))
                 (= (cdr size1) (cdr size2))))
    (loop for m1-row in matrix1
          for m2-row in matrix2
          collect (basic-list-list+ m1-row m2-row))))

  
;;;; matrix subtraction
(defgeneric matrx-sub (matrix1 matrix2)
  (:documentation "m1 - m2"))

(defmethod matrix-sub ((matrix1 list) (matrix2 list))
  "m1-m2"
  (let ((size1 (lists-length-equal matrix1))
        (size2 (lists-length-equal matrix2)))
    (assert (and size1 size2))
    (assert (and (= (car size1) (car size2))
                 (= (cdr size1) (cdr size2))))
    (loop for i in matrix1
          for j in matrix2
          collect (basic-list-list- i j))))

               
;;;; matrix multiply a scalar
(defgeneric matrix-multiple-scalar (matrix n)
  (:documentation " matrix * number"))

(defmethod matrix-multiple-scalar ((matrix list) (n number))
  "matrix * number"
  (loop for row in matrix
        collect
        (basic-list-scalar* row n)))


;;;; matrix divide a scalar
(defgeneric matrix-divide-scalar (matrix n)
  (:documentation " matrix / number"))

(defmethod matrix-divide-scalar ((matrix list) (n number))
  "matrix * number"
  (loop for row in matrix
        collect
        (basic-list-scalar/ row n)))

  
;;;; matrix multiply matrix
(defgeneric rank-one-matrix-product (col-vec row-vec)
  (:documentation "n*1 vector multiply 1*n vector"))

(defmethod rank-one-matrix-product ((col-vec list) (row-vec list))
  "n*1 vector multiply 1*n vector, list type, eg. ((1 2 3))' * ((1 2 3))"
  (basic-list-list* (car col-vec) (car row-vec)))

(defgeneric matrix-product (matrix1 matrix2)
  (:documentation "product of two matrix matrix1 and matrix2"))

(defmethod matrix-product ((matrix1 list) (matrix2 list))
  "matrix product: m1 * m2"
  (let ((size1 (lists-length-equal matrix1))
        (size2 (lists-length-equal matrix2)))
    (assert (and size1 size2))
    (assert (= (cdr size1) (car size2)))
    (reduce #'matrix-add
            (loop for row in matrix1
                  for col in (transpose matrix2)
                  collect (basic-list-list* row col)))))


;;;; inner produce
(defgeneric inner-product (a b)
  (:documentation "inner product of vector a and b"))

(defmethod inner-product ((a list) (b list))
  "inner product, '((1 2 3)) dot '((1 2 3))"
  (assert (list-length-equal (car a) (car b)))
  (assert (list-check-type (car a) 'number))
  (assert (list-check-type (car b) 'number))
  (loop for i in (car a)
        for j in (car b)
        sum (* i j)))

;;;; matrix size
(defgeneric matrix-size (m)
  (:documentation "return the size of a matrix or vector, (rows . cols) , for a row vector, return (1 . cols)"))

(defmethod matrix-size ((m list))
  "Return a cons (rows . cols). If m is not rectangle, return nil."
  (lists-length-equal m))

;;;; ith row of a matrix
(defgeneric nth-row (matrix nth)
  (:documentation "return the nth row of matrix, return a row vector, '((1 2 3))"))

(defmethod nth-row ((matrix list) (n-th integer))
  "return the nth row of matrix"
  (assert (> (car (matrix-size matrix)) n-th))
  (nth n-th matrix))

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
  (:documentation "row elimination, the resule is an upper triangle"))

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
  "row elimination, for all columns!"
  (let ((size (matrix-size matrix)))
    (row-elimination-up-down%% matrix (min (car size) (cdr size)))))


;;;; row elimination, down to up, the input matrix is an upper triangle matrix, and the result is an diag matrix
(defgeneric row-elimination-down-up (matrix)
  (:documentation "row elimination, the resule is an diag triangle matrix"))

;;TO DO


  
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


;;;; Gauss-Jordan Elimination
(defgeneric gauss-jordan-elimination (matrix)
  (:documentation "Calculating 1/A by Gauss-Jordan Elimination"))

 
;;;; construct a matrix
(defgeneric make-zeros (m &optional n)
  (:documentation "return a matrix which elements are all 0's. If n is nil, return a row vector such as '((0 0 0))"))

(defmethod make-zeros ((m integer) &optional n)
  "return a full zero matrix"
  (if (null n)
      (list (loop for i below m collect 0))
      (loop for row below m
            collect (loop for col below n
                          collect 0))))

(defgeneric make-ones (m &optional n)
  (:documentation "return a matrix which elements are all 1's. If n is nil, return a row vector such as '((1 1 1))"))

(defmethod make-ones ((m integer) &optional n)
  "return a full zero matrix"
  (if (null n)
     (list (loop for i below m collect 1))
      (loop for row below m
            collect (loop for col below n
                          collect 1))))

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

(defgeneric eye (n)
  (:documentation "make an identity matrix with rank n"))

(defmethod eye ((n integer))
  "identity matrix"
  (loop for row below n
        collect (loop for col below n
                      collect (if (= row col) 1 0))))

(defgeneric diag (&rest diags)
  (:documentation "make a diagonal matrix"))

(defmethod diag (&rest diags)
  "make a diag matrix, parameters are the diags"
  (let ((n (length diags)))
    (loop for row below n
          for diag in diags
          collect (loop for col below n
                        collect (if (= row col) diag 0)))))


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
                             collect (random min))))
        ((and min max)
         (assert (> max min))
         (loop for row from 0 below m
               collect (loop for col from 0 below n
                             collect (+ min (random (- max min))))))
        (t (error "max < min"))))
                          
            

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

(defgeneric matrix-trace (matrix)
  (:documentation "Trace (sum of diagonal elements) of matrix"))

(defmethod matrix-trace ((matrix list))
  "trace of matrix"
  (apply #'+ (mapcar #'car (loop for row    in   matrix
                                 for row-id from 0
                                 collect (loop for col    in   row
                                               for col-id from 0
                                               when (= row-id col-id)
                                                 collect col)))))

;;;; singular
(defgeneric singular-p (square)
  (:documentation "check if a square matrix is singular, if it's singular, return t, else nil"))

(defmethod singular-p ((square list))
  "check if a square matrix is singular, if it's singular, return t, else nil"
  (assert (matrix-square-p square))
  (when (= (det square) 0) t))


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
