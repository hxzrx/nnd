;;;; Chapter 15, Dynamic Networks

(in-package :nnd)


(defclass layer ()
  ((id :initarg :id :accessor id :type integer :documentation "the layer ID")
   (neurons :initarg :neurons :accessor neurons :type integer :documentation "num of neurons of this layer")
   (network-inputs :initarg :network-input :accessor network-input :type list :initform nil
                   :documentation "$p^l$, an associate list of TDL about the inputs of the network, the key of the associate list should be according to the slot of input-weights, a layer can have multiple inputs, if it has no delays the length of the tdl should be <1> content and `delay-from <0>")
   (network-input-weights :initarg :network-input-weight :accessor network-input-weight :type list :initform nil
                  :documentation "$IW^{m,l}$, an associate list of TDL abouts the weights that receive the network's inputs, since a layer can receive multi-inputs, the key of the associate list is <l> that denote the l-th inputs of the network")
   (layer-inputs :initarg :layer-input :accessor layer-inputs :type list :initform nil
                 :documentation "an associate list of TDL abouts the inputs from the layers's inpts, the key of the associate should be according to the slot of layer-weights")
   (layer-weights :initarg :layer-weights :accessor layer-weights :type list :initform nil
                  :documentation "$LW^{m,l}$, an associate list of TDL abouts the weights that receive the layers' inputs, the key of the associate list is other layers' layer-id")
   (bias :initarg :bias :accessor bias :type (or list number) :documentation "$b^m$, bias of this layer")
   (net-input :initarg :net-input :accessor net-input :type (or list number) :initform nil
              :documentation "$n^m$, the net input of this layer")
   (transfer :initarg :transfer :accessor transfer :type function :documentation "$f^m$, transfer function of this layer")
   (neuro-output :initarg :neuro-output :accessor neuro-output :type (or list number) :initform nil
                 :documentation "$a^m$, the neuro output of this layer")
   #+:ignore(input-indices :initarg :input-indices :accessor inpyt-indices :type list :initform nil
                           :documentation "$I_m$, the list of indices of input vectors that connect this layer")
   (link-to :initarg :link-to :accessor link-to :type list :initform nil
            :documentation "the id of the layers that this layer connect to and send the neuro-output to these layers")
   (link-forward :initarg :link-forward :accessor link-forward :type list :initform nil
               :documentation "$L_m^f$, a list of id of layers that directly connect forward to this layer")
   (link-backward :initarg :link-backward :accessor link-backward :type list :initform nil
                  :documentation "$L_m^b$, a list of indices of layers that are directly connected backwards to this layer (or to which this layer connects forward) and that contain no delays in the connection")

   )
  (:documentation "A layer of a dynamic network."))

(defmethod make-layer ((layer layer) (config list))
  "should use a config to initialize this instance"
  )

(defmethod add-network-input-to-layer ((layer layer) raw-input)
  "add the network's raw input to the network-input's tdl"
  (with-slots ((input network-inputs)) layer
    (loop for (lth-in tdl) in input
          do (alexandria:when-let (in-vec (assoc lth-in raw-input))
               (add-tdl-content tdl (second in-vec))))))

(defmethod get-layer-input ((layer layer) (input-layer-id integer))
  "get the layer input tdl"
  (with-slots ((li layer-inputs)) layer
    (second (assoc input-layer-id li))))

(defmethod get-layer-weight ((layer layer) (input-layer-id integer))
  "get the layer weight tdl"
  (with-slots ((lw layer-weights)) layer
    (second (assoc input-layer-id lw))))

(defmethod get-layer-network-input-weight ((layer layer) (l-th-input integer))
  "get the network input weight tdl"
  (with-slots ((niw network-input-weights)) layer
    (second (assoc l-th-input niw))))

(defmethod get-layer-bias ((layer layer))
  (bias layer))

(defmethod get-neuro-output ((layer layer))
  (neuro-output layer))

(defmethod set-neuro-output! ((layer layer) value)
  (setf (neuro-output layer) value))

(defmethod calc-layer-net-input ((layer layer))
  "calc the net output of layer m at time t, the equation is (14.1)"
  (matrix-multi-add ;will remove nil first
          (list
           (with-slots ((lf link-forward)
                        (li layer-inputs)
                        (lw layer-weights)) layer
             (matrix-multi-add
              (loop for l in lf
                    collect (matrix-multi-add
                             (loop for delay-input in (get-tdl-content (get-layer-input l li))
                                   for delay-weight in (get-tdl-content (get-layer-weight l lw))
                                   collect (matrix-product delay-weight delay-input))))))
           (with-slots ((ni  network-inputs) ;may produce nil
                        (niw network-input-weights)) layer
             (matrix-multi-add
              (loop for (l delay-input) in ni ;ni is an associate list about input id and it's tdl
                    collect (matrix-multi-add
                             (loop for each-delay-input in (get-tdl-content delay-input)
                                   for each-delay-weight in (get-tdl-content (second (assoc l niw)))
                                   collect (matrix-product each-delay-weight each-delay-input))))))
           (bias layer))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass lddn ()
  ((layers :initarg :layers :accessor layers :type list :initform nil
           :documentation "a associate list of the layers of the network, the key of the associate list is the layer's id")
   (raw-input-layers :initarg :raw-input-layers :accessor raw-input-layers :type list :initform nil
                         :documentation "the id of the layers that receive network's raw input")
   (final-output-layers :initarg :final-output-layers :accessor :final-output-layers :type list :initform nil
                          :documentation "the id of the layers whose neuro-outputs will be used to compared with the target")
   (network-output :initarg :network-output :accessor network-output :type list :initform nil
                   :documentation "an associate list of output of the lddn network, the keys in the associate list should be across the slot of network-output-layers")
   )
  (:documentation "Layered Digital Dynamic Network, the slot's names should reference to page 290, Chinese edition"))

(defmethod get-layer ((lddn lddn) layer-id)
  "get the layer instance whose is is `layer-id"
  (with-slots ((layers layers)) lddn
    (second (assoc layer-id (layers lddn)))))

(defmethod calc-neuro-output! ((lddn lddn) (layer layer))
  "calc the output of this layer, and send the result to the layers it connects to"
  (let ((res (funcall (transfer layer) (calc-layer-net-input layer))))
    (with-slots ((layer-in layer-inputs)) lddn
      (set-neuro-output layer res)
      (loop for send-to-id in (link-to layer)
            do (add-tdl-content (second (assoc send-to-id layer-in)) res))
      res)))

(defmethod calc-lddn-output! ((lddn lddn) input-list)
  "calc the output of the lddn network providing a list of input vectors"
  (with-slots ((layers layers)
               (input-layers raw-input-layers)
               (output-layers final-output-layers)) lddn
    (dolist (id input-layers) ;send input vector to the raw input layers
      (add-network-input-to-layer (get-layer lddn id) input-list))
    (dolist (layer (sort layers #'< :key #'first)) ;calc the nuron outputs layer by layer
      (calc-neuro-output! lddn layer))
    (loop for out-layer-id in output-layers ;collect network output result
          collect (list out-layer-id (get-neuro-output (get-layer lddn out-layer-id))))))












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
