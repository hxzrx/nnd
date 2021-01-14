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
      
(defun lists-square-p (lists)
  "check the matrix is square, if square, return the size n, else return nil"
  (let ((size (lists-length-equal lists)))
    (when (and size (= (car size) (cdr size)))
      (car size))))

(defun matrix-square-p (matrix)
  "check the matrix is square, if square, return the size n, else return nil"
  (lists-square matrix))

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
  (if (null matrix) t
      (and (zeros-row-p (car matrix)) (zeros-p (cdr matrix)))))

;;;; utils for transfer functions
(defun collect-transfer (trans-fun col-vec)
  "apply the transfer function to each of the column vector"
  (assert (= 1 (cdr (lists-length-equal col-vec))))
  (loop for row in col-vec
        collect (loop for elem in row
                      collect (funcall trans-fun elem))))

(defun print-matrix (m)
  "print matrix"
  (format  t "~{~{~d  ~}~&~}~%~%" m))

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
