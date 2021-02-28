(in-package :nnd)
;;;; unsafe-fifo class and the methods are copied from dbmcclain's repos
;;;; https://github.com/dbmcclain/Lisp-Actors/tree/main/data-objects
(defclass unsafe-fifo ()
  ((hd  :accessor unsafe-fifo-hd
        :initarg  :hd)
   (tl  :accessor unsafe-fifo-tl
        :initarg  :tl)))

(defmethod initialize-instance :after ((fifo unsafe-fifo) &key &allow-other-keys)
  (let ((empty (list nil)))
    (setf (unsafe-fifo-hd fifo) empty
          (unsafe-fifo-tl fifo) empty)))

(defun make-unsafe-fifo ()
  (make-instance 'unsafe-fifo))

(defun copy-unsafe-fifo (f)
  (let ((fcopy (copy-list (unsafe-fifo-hd f))))
    (make-instance 'unsafe-fifo
                   :hd fcopy
                   :tl (last fcopy))))

(defmethod addq ((q unsafe-fifo) item &key &allow-other-keys)
  (with-accessors ((tl  unsafe-fifo-tl)) q
    (declare (cons tl))
    (setf tl (setf (cdr tl) (list item)))
    ))

(defmethod popq ((q unsafe-fifo) &key &allow-other-keys)
  (with-accessors ((hd  unsafe-fifo-hd)
                   (tl  unsafe-fifo-tl)) q
    (declare (cons hd tl))
    (if (eq hd tl)
        (values nil nil)
        (values (shiftf (car (setf hd (cdr hd))) nil) ;; keep GC happy
                t))
    ))

(defmethod peekq ((q unsafe-fifo) &key &allow-other-keys)
  (with-accessors ((hd  unsafe-fifo-hd)
                   (tl  unsafe-fifo-tl)) q
    (declare (cons hd tl))
    (if (eq hd tl)
        (values nil nil)
      (values (cadr hd) t))
    ))

(defmethod emptyq-p ((q unsafe-fifo))
  (with-accessors ((hd  unsafe-fifo-hd)
                   (tl  unsafe-fifo-tl)) q
    (eq hd tl)))

(defmethod contents ((q unsafe-fifo))
  (with-accessors ((hd  unsafe-fifo-hd)
                   (tl  unsafe-fifo-tl)) q
    (declare (cons hd tl))
    (shiftf (cdr (setf tl hd)) nil)))

(defmethod set-contents ((q unsafe-fifo) lst)
  (with-accessors ((hd  unsafe-fifo-hd)
                   (tl  unsafe-fifo-tl)) q
    (declare (cons hd tl))
    (setf (cdr hd) lst
          tl       (last hd)) ))

(defsetf contents set-contents)

(defmethod get-contents ((q unsafe-fifo))
  (with-accessors ((hd unsafe-fifo-hd)) q
    (cdr hd)))

(defmethod findq ((q unsafe-fifo) val &rest args)
  (with-accessors ((hd  unsafe-fifo-hd)) q
    (declare (cons hd))
    (apply 'find val (cdr hd) args)))

(defmethod lastq ((q unsafe-fifo))
  (with-accessors ((tl unsafe-fifo-tl)) q
    (car tl)))

(defmethod countq ((q unsafe-fifo))
  (with-accessors ((hd  unsafe-fifo-hd)) q
    ;;(declare (cons hd tl))
    (length (cdr hd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass fixed-len-unsafe-fifo (unsafe-fifo)
  ((len :initarg :len
        :accessor len
        :type integer))
  (:documentation "the unsafe fifo but has fixed length"))

(defun make-fixed-len-unsafe-fifo (n &key content)
  (make-instance 'fixed-len-unsafe-fifo :len n :init-content content))

(defmethod initialize-instance :after ((fifo fixed-len-unsafe-fifo) &key init-content &allow-other-keys)
  (with-accessors ((tl unsafe-fifo-tl)) fifo
    (declare (cons tl))
    (dotimes (i (len fifo))
      (addq fifo init-content))))
      ;;(setf tl (setf (cdr tl) (list nil))))))

(defmethod addq ((q fixed-len-unsafe-fifo) item &key &allow-other-keys)
  (when (>= (countq q) (len q)) (popq q))
  (with-accessors ((tl unsafe-fifo-tl)) q
    (declare (cons tl))
    (setf tl (setf (cdr tl) (list item)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Tapped Delay Line class

(defclass tdl ()
  ((content :initarg :content
            :accessor content
            :type fixed-len-unsafe-fifo
            :documentation "a fixed length fifo, when a new element comes, the oldest element should go out")
   (from :initarg :from
         :accessor from
         :type integer
         :initform 0
         :documentation "the series start from, according to page 291(Chinese ed.), it can be either 0 or 1, 0 means no delay")
   (tdl-type :initarg :tdl-type
         :accessor tdl-type
         :type keyword
         :initform :forward
         :documentation "denote the type of this tdl, 3 types {:forward :backward :self}"))
  (:documentation "Tapped Delay Line. The input signal enters from the left.
At the output of the tapped delay line we have an R-dimensional vector,
consisting of the input signal at the current time and at delays of from 1 to R-1 time steps, the 0th step meams no delay"))

(defun make-tdl (len &key (init-element 0) (from 0))
  "make an `len dimensional tapped delay line, with the initial element with the default value"
  (make-instance 'tdl :content (make-fixed-len-unsafe-fifo len :content init-element)
                      :from from))

(defmethod get-tdl-content ((tdl tdl))
  "get the contents of a tapped delay line, the result is a list of the tdl's values, only return the efficient content. Note that the fifo queue is in inversed order, and this method returns normal order"
  (reverse  (get-contents (content tdl))))

(defmethod add-tdl-content ((tdl tdl) item)
  (with-slots ((content content)) tdl
    (addq content item)))

(defmethod tdl-dimension ((tdl tdl))
  "get the dimension of tdl"
  (length (get-tdl-content tdl)))

(defmethod tdl-length ((tdl tdl))
  (tdl-dimension tdl))

(defmethod add-tdl ((tdl tdl) new-val)
  "new input to the tapped-delay-line"
  (addq (content tdl) new-val))

(defmethod tdl-to-vector ((tdl tdl))
  "convert tdl's fifo to a column vector, it the length of fifo is 1, return the number"
  (list-to-vector (get-tdl-content tdl)))
