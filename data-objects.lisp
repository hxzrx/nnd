(in-package :nnd)
;;;; unsafe-fifo class and the methods are copied from dbmcclain's repos
;;;; https://github.com/dbmcclain/Lisp-Actors/tree/main/data-objects
(defclass unsafe-fifo ()
  ((hd  :accessor unsafe-fifo-hd
        :initarg  :hd)
   (tl  :accessor unsafe-fifo-tl
        :initarg  :tl)))

(defmethod initialize-instance :after ((fifo unsafe-fifo) &key &allow-other-keys)
  (format t "ssssss~%")
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


(defclass fixed-len-unsafe-fifo (unsafe-fifo)
  ((len :initarg :len
        :accessor len
        :type integer))
  (:documentation "the unsafe fifo but has fixed length"))

(defun make-fixed-len-unsafe-fifo (n)
  (make-instance 'fixed-len-unsafe-fifo :len n))

(defmethod initialize-instance :after ((fifo fixed-len-unsafe-fifo) &key &allow-other-keys)
  (format t "len = ~d~%" (len fifo))
  (with-accessors ((tl unsafe-fifo-tl)) fifo
    (declare (cons tl))
    (dotimes (i (len fifo))
      (addq fifo i))))
      ;;(setf tl (setf (cdr tl) (list nil))))))

(defmethod addq ((q fixed-len-unsafe-fifo) item &key &allow-other-keys)
  (when (>= (countq q) (len q)) (popq q))
  (with-accessors ((tl unsafe-fifo-tl)) q
    (declare (cons tl))
    (setf tl (setf (cdr tl) (list item)))
    ))