;;;; Chapter 15, Dynamic Networks

(in-package :nnd)

(defclass lddn ()
  ((network-inputs :initarg :network-input :accessor network-input :type list :initform nil
                  :documentation "$p^l(t)$, the list of the lddn's input vectors")
   (net-inputs :initarg :net-input :accessor net-input :type list :initform nil
              :documentation "%n^m(t)%, net input for layer m")
   (transfers :initarg :transfer :accessor transfer :type list :initform nil
             :documentation "$f^m()$, the list of transfer functions for layer m")
   (layer-output :initarg :layer-output :accessor layer-output :type list :initform nil
                 :documentation "$a^m(t)$the output for layer m")
   (input-weights :initarg :input-weights :accessor input-weights :type list :initform nil
                  :documentation "$IW^{m,l}$, the input weight between input l and layer m")
   (layer-weights :initarg :layer-weights :accessor layer-weights :type list :initform nil
                  :documentation "$LW^{m,l}$, the layer weight between layer l and layer m")
   (biases :initarg :biases :accessor biases :type list :initform nil
           :documentation "$b^m$, the bias vector for layer m")
   (delay-links :initarg :delay-links :accessor delay-links :type list :initform nil
                :documentation "$DL_{m,l}$, the set of all delays in the tapped delay line between Layer l and Layer m")
   (delay-inputs :initarg :delay-inputs :accessor delay-inputs :type list :initform nil
                 :documentation"$DI_{m,l}$, the set of all delays in the tapped delay line between Input l and Layer m")
   (network-input-indices :initarg :network-input-indices :accessor network-input-indices :type list :initform nil
                          :documentation "$I_m$, the set of indices of input vectors that connect to layer m")
   (links-forward :initarg :links-forward :accessor links-forward :type list :initform nil
                  :documentation "$L_m^f$, the set of indices of layers that directly connect forward to layer m")
   (links-backward :initarg :links-backward :accessor links-backward :type list :initform nil
                   :documentation "$L_m^b$, the set of indices of layers that are directly connected backwards to layer m (or to which layer m connects forward) and that contain no delays in the connection")

   )
  (:documentation "Layered Digital Dynamic Network, the slot's names should reference to page 290, Chinese edition"))


;; DO NOT directly access lddn's slots
(defmethod get-nth-network-input ((network lddn) (n integer))
  (with-slots ((inputs network-inputs)) network
    (nth n inputs)))

(defmethod get-nth-layer-bias ((network lddn) (n integer))
  (with-slots ((bias biases)) network
    (nth n bias)))

(defmethod set-nth-layer-bias ((network lddn) (n integer) (new-bias list))
  (with-slots ((bias biases)) network
    (setf (nth n bias) new-bias)))

(defmethod get-nth-transfer-function ((network lddn) (n integer))
  (with-slots ((transfer transfers)) network
    (nth n transfer)))


















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; demos, examples, and exercises

(defun FIR-demo ()
  "Finite Impulse Response Network Demonstration. Page 272, Chinese edition."
  (let ((tdl (make-tdl 3 :init-element 0 :from 0))
        (input-weight '((1/3 1/3 1/3)))
        (bias 0)
        (transfer #'purelin)
        (network-output nil)
        (square-wave (square-wave-generator 1 10)))
    (loop for i from 0 to 19
          do (progn
               (add-tdl tdl (funcall square-wave))
               (setf network-output (funcall transfer
                                             (reduce #'matrix-add
                                                     (list (matrix-product input-weight (tdl-to-vector tdl))
                                                           bias))))
               (format t "~&~d~d~,3f~%" i #\tab network-output)))))


(defun IIR-demo ()
  "Infinite Impulse Response Network Demonstration. Page 271 Chinese edition."
  (let ((tdl (make-tdl 1 :init-element 0))
        (input-weight 1/2)
        (layer-weight 1/2)
        (bias 0)
        (transfer #'purelin)
        (network-input nil)
        (network-output nil)
        (square-wave (square-wave-generator 1 10)))
    (loop for i from 0 to 19
          do (progn
               (setf network-input (funcall square-wave))
               (setf network-output (funcall transfer
                                         (reduce #'matrix-add
                                                 (list (matrix-product input-weight network-input)
                                                       (matrix-product layer-weight (tdl-to-vector tdl))
                                                       bias))))
               (add-tdl tdl network-output) ; make a delay
               (format t "~&~d~d~,3f~%" i #\tab network-output)))))
