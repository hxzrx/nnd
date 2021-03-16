
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

(defun neurons-from-weights (weights)
  "for a R-S¹-S²-...-Sᴹ network, return (list R S¹ S² ... Sᴹ) when the network's weights is given"
  (cons (cdr (matrix-size (car weights)))
        (loop for w in weights
              collect (car (matrix-size w)))))

(defun restore-matrices-rank% (lst &optional (accu nil))
  "'(2 3 1 3) -> ((3 2) (1 3) (3 1))"
  (if (null (third lst))
      (cons (list (second lst) (first lst)) accu)
      (restore-matrices-rank% (cdr lst ) (cons (list (second lst) (first lst)) accu))))

(defun restore-matrices-rank (lst)
  "'(2 3 1 3) -> ((3 2) (1 3) (3 1))"
  (reverse (restore-matrices-rank% lst)))

(defun neurons-to-random-weights (neurons &optional (min -0.5) (max 0.5)) ;-0.5 to 0.5 is suggested in exercise 11.25, page 209, Chinese edition
  "make a list of weights the neurons are provided for each layer, this function is used in initializing a network randomly"
  (loop for (row col) in (restore-matrices-rank neurons)
        collect (rand-matrix row col min max)))

(defun neurons-to-random-biases (neurons &optional (min -0.5) (max 0.5))
  "make a list of biases the neurons are provided for each layer, this function is used in initializing a network randomly"
  (loop for (row nil) in (restore-matrices-rank neurons)
        collect (rand-matrix row 1 min max)))

(defun column-vector-to-list (vec)
  "convert a column vector to a list, if vec is a number, return the number itself"
  (if (column-vector-p vec)
    (if (listp vec)
        (first (transpose vec))
        vec) ;the number case
    (format t "Warning: ~d is not a vector!~%" vec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun boxmuller-sampling (&optional (mu 0) (sigma 1))
  "get a normal distribution random number by Box-Muller method
https://www.zhihu.com/question/29971598
def boxmullersampling(mu=0, sigma=1, size=1):
    u = np.random.uniform(size=size)
    v = np.random.uniform(size=size)
    z = np.sqrt(-2*np.log(u))*np.cos(2*np.pi*v)
    return mu+z*sigma
"
  (let* ((u (random 1.0))
         (v (random 1.0))
         (z (* (sqrt (* -2 (log u)))
               (cos (* 2 pi v)))))
    (+ mu (* z sigma))))

(defgeneric gauss-random (min max)
  (:documentation "Get a normal distribution random number between min and max."))

(defmethod gauss-random ((min real) (max real))
  (cond ((< min max)
         (alexandria:gaussian-random min max))
        ((> min max)
         (alexandria:gaussian-random max min))
        (t min))) ;if min=max, the algorithm will take a very long time, see the doc of alexandria

(defmethod gauss-random ((vec1 list) (vec2 list))
  "gauss random number between two column vetors, the rank of the two vectors should be equal"
  (let ((rank1 (matrix-size vec1))
        (rank2 (matrix-size vec2)))
    (assert (equal rank1 rank2))
    (loop for r from 0 below (car rank1)
          collect (loop for c from 0 below (cdr rank1)
                        collect (gauss-random (nth c (nth r vec1))  (nth c (nth r vec2)))))))

(defgeneric rand-between (vec1 vec2)
  (:documentation "uniform random vector or random number between the two numbers"))

(defmethod rand-between ((num1 real) (num2 real))
  "uniform random number between mum1 and mum2"
  (if (= num1 num2) num1 (+ (min num1 num2) (random (abs (* 1.0 (- num1 num2)))))))

(defmethod rand-between ((vec1 list) (vec2 list))
  "uniform random number between two column vetors, the rank of the two vectors should be equal"
  (let ((rank1 (matrix-size vec1))
        (rank2 (matrix-size vec2)))
    (assert (equal rank1 rank2))
    (loop for r from 0 below (car rank1)
          collect (loop for c from 0 below (cdr rank1)
                        collect (rand-between (nth c (nth r vec1))  (nth c (nth r vec2)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun data-generator-accurate (gen-fun min-vec max-vec gen-num &key type)
  "generate a list of data, given a function and it's input intervals as well as how many data we need, no noise!
min-vec and max-vec should be column vector,
the result will convert to a well formed such as (list '((p11 p12) (a11 a12)) '((p21 p22) (a21 a22)) ...)
eg. (data-generator-accurate #'(lambda (x) (1+ (sin (* (/ pi 4) x)))) -2 2 11 :type :uniform)"
  (ecase type
    (:uniform (loop for input in (matrix-slice min-vec max-vec gen-num) ;input is a column vector
                    collect (list (column-vector-to-list input)
                                  (column-vector-to-list (funcall gen-fun input)))))
    (:random (loop for i from 0 below gen-num
                   for x = (rand-between min-vec max-vec)
                   collect (list (column-vector-to-list x)
                                 (column-vector-to-list (funcall gen-fun x)))))))

(defun data-generator-with-noise (gen-fun min-vec max-vec gen-num noise-function noise-min noise-max &key type)
  "generate a list of data, given a function and it's input intervals as well as how many data we need, with noise, i.i.d.!
min-vec and max-vec should be column vector,
the result will convert to a well formed such as (list '((p11 p12) (a11 a12)) '((p21 p22) (a21 a22)) ...)
noise-function is a function which receive two parameters of the type number, min and max, and generate a random number between them,
eg. (data-generator-with-noise #'(lambda (x) (1+ (sin (* (/ pi 2) x)))) -2 2 5 #'rand-between -0.1 0.1 :type :random)"
  (flet ((add-noise (lst min max) ;lst is either a list or a number
           (if (listp lst)
               (loop for i in lst
                     collect (+ i (funcall noise-function min max)))
               (+ lst (funcall noise-function min max)))))
    (ecase type
      (:uniform
       (loop for input in (matrix-slice min-vec max-vec gen-num) ;input is a column vector
             collect (list (column-vector-to-list input)
                           (add-noise (column-vector-to-list (funcall gen-fun input))
                                      noise-min noise-max))))
      (:random
       (loop for i from 0 below gen-num
             for x = (rand-between min-vec max-vec)
             collect (list (column-vector-to-list x)
                           (add-noise (column-vector-to-list (funcall gen-fun x))
                                      noise-min noise-max)))))))

(defun data-generator-gauss-noise (gen-fun min-vec max-vec gen-num noise-min noise-max &key type)
  "eg. (data-generator-gauss-noise #'(lambda (x) (1+ (sin (* (/ pi 2) x)))) -2 2 5 -0.1 0.1 :type :random)"
  (data-generator-with-noise gen-fun min-vec max-vec gen-num #'gauss-random noise-min noise-max :type type))

(defun data-generator-uniform-noise (gen-fun min-vec max-vec gen-num noise-min noise-max &key type)
  "eg. (data-generator-uniform-noise #'(lambda (x) (1+ (sin (* (/ pi 2) x)))) -2 2 5 -0.1 0.1 :type :random)"
  (data-generator-with-noise gen-fun min-vec max-vec gen-num #'rand-between noise-min noise-max :type type))

(defun data-generator-uniform-noise (gen-fun min-vec max-vec gen-num noise-min noise-max &key type)
"eg. (data-generator-uniform-noise #'(lambda (x) (1+ (sin (* (/ pi 2) x)))) -2 2 5 -0.1 0.1 :type :random)"
  (data-generator-with-noise gen-fun min-vec max-vec gen-num #'rand-between noise-min noise-max :type type))

(defun noise-generator (noise-function noise-min noise-max)
  "return a generator function that produce noise by `nose-fun, it can produce noise of type number or vector depending on the type of `noise-min"
  #'(lambda () (funcall noise-function noise-min noise-max)))

(defun gauss-noise-generator (noise-min noise-max)
  "return a generator function that produce gauss noise between noise-min and noise-max"
  (noise-generator #'gauss-random noise-min noise-max))

(defun uniform-noise-generator (noise-min noise-max)
  "return a generator function that produce uniform noise between noise-min and noise-max"
  (noise-generator #'rand-between noise-min noise-max))

(defun square-wave-generator (amplitude period)
  "make an generator that produce discrete square wave, period should be an even integer"
  (if (= (mod period 2) 1) (warn "the period should be an even number"))
  (let ((start -1))
    #'(lambda ()
        (let ((i (incf start)))
          (if (= (mod (- i (mod i (/ period 2))) 2) 0)
              amplitude (* -1 amplitude))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-to-vector (lst)
  "conver a list of numbers to a column vector, if `lst is a number, return the number itself"
  (if (listp lst)
      (if (= (length lst) 1)
          (car lst)
          (transpose (list lst)))
      lst))

(defmacro if-typep-let ((var test-form) type-test then-form &optional else-form)
  "(if-typep-let (x #'+) #'functionp 'a-function 'not-a-function)"
  `(let ((,var ,test-form))
     (if (funcall , type-test ,var) ,then-form ,else-form)))

(defun find-function (symbol &optional (package :nnd))
  "return a function with a symbol which has the same name as the returned function"
  (symbol-function (find-symbol (string-upcase (symbol-name symbol)) package)))

(defun make-delay-from-config (delay-config init-val)
  "Return a tdl instance with the config such as (list :from 1 :to 1 :dir :self), see page 291, Chinese edition.
TDLs that are of the type self or backward and from zero delay are not considered yet, since they mean a(t) = f(a(t), ...).
TDLs that are of the type forward should make to+1 length of tlds , even if they start from 1."
    (let ((dir-type (getf delay-config :dir))
        (from (getf delay-config :from))
        (to (getf delay-config :to)))
      ;; :from and :to are kept the same as which in the book page 291, so one must be carefull when configuring the delay
      ;; even though if there's no delay when propagating forward, we use a tdl to keep consistence with others
      (cond ((eq dir-type :self)     (make-tdl to      :init-element init-val :from 0    :tdl-type :self)) ;delay at least 1
            ((eq dir-type :backward) (make-tdl to      :init-element init-val :from 0    :tdl-type :backward))
            ((eq dir-type :forward)  (make-tdl (1+ to) :init-element init-val :from from :tdl-type :forward))
            (t                       (make-tdl 1       :init-element init-val :from 0    :tdl-type :forward))) ;forward link, no delay
      ))

(defmacro getf->> (nested-plist &rest keys)
  "recursive applying getf to the final nalue of a nested property list
eg. (getf-> '(:a (:b (:c 1))) :a :b :c)  ->  1"
  (if keys
      `(getf->> (getf ,nested-plist ,(car keys)) ,@(cdr keys))
      nested-plist))

(defmacro ->> (arg &rest fn-forms)
  (if fn-forms
      `(->> (funcall ,(car fn-forms) ,arg) ,@(cdr fn-forms))
      arg))

(defun list-interpolation (lst item &optional (accu nil))
  "interpolation `item between lst"
  (if (cdr lst)
      (list-interpolation (cdr lst) item (append (append accu (list (car lst))) (list item)))
      (append accu (list (car lst)))))

(defun id-tdl-alist-format (alist &optional (id-cap "ID") (tdl-cap "TDL"))
  "return a string of describing the alist of id and tdl pairs, used to pretty print lddn-layer"
  (apply #'concatenate 'string
   (list-interpolation
   (loop for (id tdl) in alist
        collect (format nil "<~d: ~d  <~d:, tdl fifo length: ~d, tdl from: ~d, type: ~d>/>"
                        id-cap id tdl-cap
                        (tdl-fifo-length tdl)
                        (from tdl)
                        (tdl-type tdl)))
   " ")))

(defgeneric format-string (object)
  (:documentation "return a string about the brief information about the object, and this string can be used in print-object method to get a pretty print"))

#+:ignore
(defun alist-push-or-replace! (alist item replaced-to &key (test #'eql))
  "assoc an alist, if return t, replace it's value to `replaced-to, else push an new cons to the alist whose key is `item and value is `replaced-to. Note that this function does not check if item is nil, do it in the invoking place if needed"
  (alexandria:if-let (assoc-res (assoc item alist :test test))
    (rplacd assoc-res (list replaced-to))
    (nconc alist (list (list item replaced-to))))
  alist)

#+:ignore
(defun alist-create-or-adjoin! (alist key new-value)
  "assoc an alist, if return t, adjoin `value to the respected val-list, else push a new k/v pair, return a new alist.
  alist is something like '((:C (1)) (:B (4 3 2))).
  be carefull when alist is nil, it will not modify the parameter, use the functional edition alist-create-or-adjoin"
  ;not tested when alist is nil or new-value is nil
  (if (null alist)
      (list (list key (list new-value)))
      (progn (alexandria:if-let (assoc-result (assoc-utils:aget alist key)) ;aget return the cdr of the cons
               (when (null (member new-value (first assoc-result)))
                 (nconc (first assoc-result) (list new-value)))
               (nconc alist (list (list key (list new-value)))))
             alist)))

(defun alist-create-or-adjoin (alist key new-value &key (test #'equal))
  "alist-create-or-adjoin! will not modify the alist parameter if it is `nil, so use this function without side effects"
  (if alist
      (if (assoc key alist)
          (loop for pair-list in alist
                collect (if (funcall test key (first pair-list))
                            (list (first pair-list) (adjoin new-value (second pair-list)))
                            pair-list))
          (append alist (list (list key
                                    (if new-value
                                        (list new-value)
                                        nil)))))
      (if new-value
          (list (list key (list new-value)))
          (list (list key nil)))))


(defun partial-derivative (fx parameter-list is-explicit)
  "`fx is a list of functions corresponding to the partial derivatives of each element of the `parameter-list,
if `is-explicit is nil return a zeros vector of length of the dimension of `parameter-list,
this function return a column vector about the partial derivatives.
note that this function only contains a very special case that each `fx's parameter is the corrsponding element of `parameter-list's, and this is enough to calculate sse.
eg. (explicit-partial-derivative (list #'sin #'sin) '(1 2) t)"
  (if is-explicit
      (transpose (list (mapcar #'funcall fx parameter-list)))
      (make-zero-vector (length parameter-list))))

(defun partial-deriv-SSE (target output is-explicit)
  "partial derivatie of SSE about "
  (if (listp target)
      (if is-explicit
          (transpose (list (loop for tq in target
                                 for aq in output
                                 collect (* -2 (- tq aq)))))
          (make-zero-vector (car (matrix-size output))))
      (if is-explicit (* -2 (- target output)) 0)))

(defun find-cdr (list item &key (test #'eql))
  "find `item' in `list' and return the cdr of the list from the place, return nil if not find any"
  (if list
      (if (eql (first list) item)
          list
          (find-cdr (cddr list) item :test test))
      nil))

(defun plist-match (template-plist query-plist &key (test #'eql))
  "return t if testing every k/v in query-plist is passed by the test in the template-plist else return nil"
  (if query-plist
      (alexandria:if-let (found (find-cdr template-plist (pop query-plist)))
        (and (funcall test (second found) (pop query-plist))
             (plist-match template-plist query-plist :test test))
        nil)
      t))

(defun find-most-id (lst &key most most-id iter-id (key #'identity) test)
  "find where the most element is with the test function `test' when comparing with key function `key'"
  (if most
      (if lst
          (if (funcall test (funcall key most) (funcall key (car lst)))
              (find-most-id (cdr lst) :most most      :most-id most-id      :iter-id (1+ iter-id) :key key :test test)
              (find-most-id (cdr lst) :most (car lst) :most-id (1+ iter-id) :iter-id (1+ iter-id) :key key :test test))
          most-id)
      (find-most-id (cdr lst) :most (car lst) :most-id 0 :iter-id 0 :key key :test test))) ;to initialize

(defun find-max-id (lst &optional (return-first t))
  "find where the max element is in a list of numbers, if `return-first' is t, return the first id when one number ties another, else reurn the last id"
  (if return-first
      (find-most-id lst :test #'>=)
      (find-most-id lst :test #'>)))

(defun repeat-list (lst n &optional (accu nil))
  "repeated and append lst for n times. eg. (repeat-list '(1 2 3) 3) -> (1 2 3 1 2 3 1 2 3)"
  (if (= n 0)
      accu
      (repeat-list lst (1- n) (append accu lst))))

(defun neighbor% (rank center radius)
  "return a rhombus shape (square rotated pi/4) of id's of `matrix' for `center' with `radius', rank and center are both list"
  (let* ((rows (first rank))
         (cols (second rank))
         (center-row (first center))
         (center-col (second center)))
    (append
     (apply #'append
            (loop for x from (- center-row radius) to center-row
                  for i from 0
                  when (and (>= x 0) (< x rows))
                    collect
                    (loop for y from (- center-col i) to (+ center-col i)
                          when (and (>= y 0) (< y cols))
                            collect (list x y))))
     (apply #'append (loop for x from (1+ center-row) to (+ center-row radius)
                           for i from (1- radius) downto 0
                           when (and (>= x 0) (< x rows))
                           collect
                           (loop for y from (- center-col i) to (+ center-col i)
                                 when (and (>= y 0) (< y cols))
                                   collect (list x y)))))))

(defgeneric neighbor (matrix center radius)
  (:documentation "collect the indices of the elements which are the neighbors of center")
  (:method ((matrix list) (center list) (radius integer))
    "for m * n matrix where center is the list of row and col subscripts of the center element of the matrix"
    (let ((size (matrix-size matrix)))
      (neighbor% (list (car size) (cdr size)) center radius)))
  (:method ((matrix list) (center integer) (radius integer))
    "for center is the i-th element in the matrix, center should between 0 and (m * n -1)"
    (let* ((size (matrix-size matrix))
           (rows (car size))
           (cols (cdr size))
           (x (mod center cols))
           (y (/ (- center x) rows))
           (polar (list x y)))
      (neighbor% (list rows cols) polar radius))))

(defmethod neighbor-id ((list-num integer) (arranged list) (center integer) (radius integer))
  "if a list is arranged as an matrix, return the original indices of the the elements of the list whore are neighbors of the center place in the matrix with a radius"
  (let* ((rows (first arranged))
         (cols (second arranged))
         (center-col (mod center cols))
         (center-row (/ (- center (mod center cols)) cols)))
    (assert (= list-num (* rows cols)))
    (append
     ;(apply #'append
            (loop for x from (- center-row radius) to center-row
                  for i from 0
                  when (and (>= x 0) (< x rows))
                    append
                    (loop for y from (- center-col i) to (+ center-col i)
                          when (and (>= y 0) (< y cols))
                            collect (+ (* x cols) y)))
            ;(apply #'append
            (loop for x from (1+ center-row) to (+ center-row radius)
                           for i from (1- radius) downto 0
                           when (and (>= x 0) (< x rows))
                             append
                             (loop for y from (- center-col i) to (+ center-col i)
                                   when (and (>= y 0) (< y cols))
                                     collect (+ (* x cols) y))))))

(defun make-layer-weights-from-list (param-list rows cols)
  "make an rows*cols matrix and cols*1 vector from a list of numbers"
  (from-list param-list rows cols))

(defun make-layer-biases-from-list (param-list rows cols)
  "make an rows*cols matrix and cols*1 vector from a list of numbers"
  (from-list param-list rows cols))
