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
  "note that the first element of the returned list is the oldest and the last one is the newest"
  (with-accessors ((hd unsafe-fifo-hd)) q
    (cdr hd)))

(defmethod get-nth-content ((q unsafe-fifo) n)
  "return the nth element of q, note the 0th is the newest and the (n-1)'th is the oldest"
  (assert (and (>= n 0) (< n (countq q))))
  (nth n (reverse (get-contents q))))

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

(defmethod add-fixed-fifo ((fifo fixed-len-unsafe-fifo) item)
  (addq fifo item))

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

(defun make-tdl (len &key (init-element 0) (from 0) (tdl-type :forward))
  "make an `len dimensional tapped delay line, with the initial element with the default value"
  (make-instance 'tdl :content (make-fixed-len-unsafe-fifo len :content init-element)
                      :from from
                      :tdl-type tdl-type))

(defmethod print-object ((tdl tdl) stream)
  (print-unreadable-object (tdl stream :type t)
    (format stream "content: ~d, from: ~d, type: ~d" (get-tdl-fifo-content tdl) (from tdl) (tdl-type tdl))))

(defmethod get-tdl-effective-content ((tdl tdl))
  "get the contents of a tapped delay line, the result is a list of the tdl's values, only return the effective content (the items not before :from). Note that the fifo queue is in inversed order, and this method returns normal order"
  (nthcdr (from tdl) (reverse (get-contents (content tdl)))))

(defmethod get-tdl-fifo-content ((tdl tdl))
  "the content of the fixed length fifo of the tdl"
  (reverse (get-contents (content tdl))))

(defmethod query-tdl-delay-base ((tdl tdl))
  (with-slots ((tdl-type tdl-type)) tdl
    (ecase tdl-type
      (:forward 0)
      (:self 1)
      (:backward 1))))

(defmethod query-tdl-delays ((tdl tdl))
  "return a list of realworld delay number about this tdl with respected to delay-type"
  (with-slots ((tdl-from from)
               (tdl-type tdl-type)) tdl
    (let ((delay-base (get-tdl-delay-base tdl)))
      (loop for i from tdl-from below (tdl-fifo-length tdl)
            collect (+ delay-base i)))))

(defmethod query-tdl-content-by-delay ((tdl tdl) delay)
  "get the content with respect to `delay, note delay is a realworld delay, so a :self type of tdl has at least one delay time"
  (with-slots ((tdl-from from)
               (tdl-type tdl-type)) tdl
    (let ((delay-base (get-tdl-delay-base tdl)))
      (nth (- delay delay-base) (get-tdl-fifo-content tdl)))))


(defmethod add-tdl-content ((tdl tdl) item)
  (with-slots ((content content)) tdl
    (addq content item)))

(defmethod tdl-effective-dimension ((tdl tdl))
  "get the effective dimension of tdl"
  (length (get-tdl-effective-content tdl)))

(defmethod tdl-fifo-dimension ((tdl tdl))
  "get the fifo dimension of tdl"
  (length (get-tdl-fifo-content tdl)))

(defmethod tdl-effective-length ((tdl tdl))
  (tdl-effective-dimension tdl))

(defmethod tdl-fifo-length ((tdl tdl))
  (tdl-fifo-dimension tdl))

#+:ignore (defmethod add-tdl ((tdl tdl) new-val)
  "new input to the tapped-delay-line, duplicated of add-tdl-content"
  (addq (content tdl) new-val))

(defmethod tdl-to-vector ((tdl tdl))
  "convert tdl's fifo to a column vector, it the length of fifo is 1, return the number"
  (list-to-vector (get-tdl-effective-content tdl)))

(defmethod set-tdl-content ((tdl tdl) new-content-list)
  "set the whole fixed length fifo's content of tdl"
  (dolist (content new-content-list)
    (add-tdl-content tdl content))
  (dotimes (i (from tdl)) ;equal to the commented form below but will applicable for :from greater than 1
    (add-tdl-content tdl nil))
  #+:ignore(when (= (from tdl) 1) ;only applicable for :forward type
             (add-tdl-content tdl nil))
  )

(defmethod get-tdl-type ((tdl tdl))
  (tdl-type tdl))

(defmethod get-tdl-from ((tdl tdl))
  (from tdl))

(defmethod tdl-has-delay? ((tdl tdl))
  "check if this tdl has at least some nonzero delay, return nil if the tdl has only a zero delay"
  (if (and (eq (get-tdl-type tdl) :forward)
           (= (tdl-fifo-length tdl) 1)) ;see make-delay-from-config for delay definition
      nil
      t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tabular-db ()
  ((db :initarg :db :accessor db :type list :initform nil :documentation "a list of property list with k/v across each list")
   ;;restrict all keys to keyword to make this db simple
   (valid-keys :initarg :valid-keys :accessor valid-keys :type list :initform nil :documentation "a list of valid-keys")
   (value-test :initarg :value-test :accessor value-test :type function :initform #'equal
         :documentation "value test function to get/setf records")
   (imp-key :initarg :imp-key :accessor imp-key :initform :rid
            :documentation "row id for each db record, implicitly to add to each record when inserting the record")
   (max-rid :initarg :max-rid :accessor max-rid :type integer :initform 0 :documentation "an auto increment number denoting the maximum row id at the present time, only 'insert' operation can modify this slot!"))
  (:documentation "a simple tabular database with each record a property list"))

(defun make-tabular-db (keys &key  (value-test #'equal))
  "keys is a list of the key in db"
  (make-instance 'tabular-db
                 :valid-keys keys
                 :value-test value-test))

(defmethod initialize-instance :after ((tdb tabular-db) &key &allow-other-keys)
  (with-slots ((vk valid-keys)
               (ik imp-key)) tdb
    (setf vk (append vk (list ik)))))

(defmethod print-object ((tdb tabular-db) stream)
  (print-unreadable-object (tdb stream :type t)
    (with-slots ((db db)) tdb
      (format stream "~&Tabular DB:~&~{~d~^~&~}~%" db))))

(defmethod get-keys ((tdb tabular-db))
  (valid-keys tdb))

(defmethod query-tabular-db ((tdb tabular-db) (query-plist list))
  "fetch all records that match query-plist, which is an property list about the query k/v's"
  (with-slots ((db db)
               (value-test value-test)) tdb
    (loop for record in db
          when (plist-match record query-plist :test value-test)
            collect record)))

(defmethod query-tabular-db-value ((tdb tabular-db) (query-plist list) target-key)
  "return the target-value with the `query-plist' and return the value respected to `target-key',
the `query-plist' should be necessary to fetch only ONE record, and this function should return only one value"
  (with-slots ((key-compare key-compare)
               (key-test key-test)) tdb
    (let* ((record (query-tabular-db tdb query-plist))
           (len (length record)))
      (cond ((= len 1) (getf (car record) target-key))
            ((= len 0) (warn "Found no records for query: ~d" query-plist))
            (t (warn "Found ~d records for query: ~d" len query-plist))))))


(defun check-keys-valid? (keys-list plist &key (test #'eql))
  "check if the keys in `plist' are all valid"
  (if plist
      (and (member (first plist) keys-list :test test)
           (check-keys-valid? keys-list (cddr plist) :test test))
      t))

(defmethod insert-tabular-db! ((tdb tabular-db) (new-record list))
  "insert a new record to tdb, `new-record' is a plist"
  (with-slots ((db db)
               (keys valid-keys)
               (rid max-rid)) tdb
    (if (check-keys-valid? keys new-record)
        (progn (setf db (cons (append (list :rid rid) new-record) db))
               (incf rid))
        (warn "there're invalid keys in the record and insert failed: ~d" new-record))
    (append (list :rid (1- rid)) new-record)))

(defun update-by-plist (raw-plist update-plist)
  "update the values in `raw-plist' from `update-plist', do not need to compare the val.
it's better to do plist-match before update-by-plist."
  (loop for (update-key . val) in (alexandria:plist-alist update-plist)
        do (setf (getf raw-plist update-key) val))
  raw-plist)

(defmethod update-tabular-db! ((tdb tabular-db) (where-plist list) (update-plist list))
  (let ((updated? nil))
    (with-slots ((db db)
                 (value-test value-test)) tdb
      (loop for record in db
            when (plist-match record where-plist :test value-test)
              do (progn (setf record (update-by-plist record update-plist))
                        (setf updated? t))))
    (values tdb updated?)))

(defmethod delete-from-tabular-db! ((tdb tabular-db) (query-plist list))
  "side effect: modify the slot of db"
  (with-slots ((db db)
               (value-test value-test)) tdb
    (setf db (loop for record in db
                   when (null (plist-match record query-plist :test value-test))
                     collect record))))

(defmethod truncate-tabular-db! ((tbd tabular-db))
  (with-slots ((db db)
               (max-rid max-rid)) tbd
    (setf db nil)
    (setf max-rid 0)))
