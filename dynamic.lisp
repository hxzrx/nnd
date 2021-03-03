;;;; Chapter 15, Dynamic Networks

(in-package :nnd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; class definition and initializing

(defclass lddn-layer ()
  ((id :initarg :id :accessor id :type integer :documentation "the layer ID")
   (neurons :initarg :neurons :accessor neurons :type integer :documentation "num of neurons of this layer")
   (network-inputs :initarg :network-inputs :accessor network-inputs :type list :initform nil
                   :documentation "$p^l$, an associate list of TDL about the inputs of the network, the key of the associate list should be according to the slot of network-input-weights, a layer can have multiple inputs, if it has no delays the length of the tdl should be <1> content and `from <0>")
   (network-input-weights :initarg :network-input-weights :accessor network-input-weights :type list :initform nil
                  :documentation "$IW^{m,l}$, an associate list of TDL abouts the weights that receive the network's inputs, since a layer can receive multi-inputs, the key of the associate list is <l> that denote the l-th inputs of the network")
   (layer-inputs :initarg :layer-inputs :accessor layer-inputs :type list :initform nil
                 :documentation "an associate list of TDL abouts the inputs from the layers's inpts, the key of the associate should be according to the slot of layer-weights")
   (layer-weights :initarg :layer-weights :accessor layer-weights :type list :initform nil
                  :documentation "$LW^{m,l}$, an associate list of TDL abouts the weights that receive the layers' inputs, the key of the associate list is other layers' layer-id")
   (bias :initarg :bias :accessor bias :type (or list number) :documentation "$b^m$, bias of this layer")
   (net-input :initarg :net-input :accessor net-input :type (or list number) :initform nil
              :documentation "$n^m$, the net input of this layer")
   (transfer :initarg :transfer :accessor transfer :type function :documentation "$f^m$, transfer function of this layer")
   (derivative-fun :initarg :derivative-fun :accessor derivative-fun :type function
                   :documentation "derivative function of the transfer function")
   (neuron-output :initarg :neuron-output :accessor neuron-output :type (or list number) :initform nil
                 :documentation "$a^m$, the neuron output of this layer")
   #+:ignore(input-indices :initarg :input-indices :accessor inpyt-indices :type list :initform nil
                           :documentation "$I_m$, the list of indices of input vectors that connect this layer")
   (link-to :initarg :link-to :accessor link-to :type list :initform nil
            :documentation "the id of the layers that this layer connect to and send the neuro-output to these layers")
   (link-forward :initarg :link-forward :accessor link-forward :type list :initform nil
               :documentation "$L_m^f$, a list of id of layers that directly connect forward to this layer")
   (link-backward :initarg :link-backward :accessor link-backward :type list :initform nil
                  :documentation "$L_m^b$, a list of indices of layers that are directly connected backwards to this layer (or to which this layer connects forward) and that contain NO DELAYS in the connection, this is a subset of `link-to but has no delays")
   )
  (:documentation "A layer of a dynamic network.
There are 4 types of slots,
1. can be initialized only by the current layer, these slots are id, neurons, bias, transfer, derivative-fun, link-to, layer-inputs, network-inputs, link-forward;
2. can be initialized by the currnt layer when the respected values are configured, or will initialize with random values by the help of the information outside of this layer, these slots are network-input-weights, layer-weights.
3. should be initialized with the information outside of the current layer, these slots  link-backward;
4. initialized with default value, but updates in the network's propagation, these slots are net-input, neuro-output.
The 2nd and 3rd types of slots will be initialized as :after in the make-instance of lddn "))

(defun make-lddn-layer (config)
  "this function should only be used in make-lddn, and it will return an lddn-layer instance from a config,
this function will only initialize the slots that do not share data outside of the layer, these slots are described in the doc of lddn-layer class"
  (let* ((id (getf config :id))
         (neurons (getf config :neurons))
         (bias (alexandria:if-let (b (getf config :bias)) b (rand-matrix neurons 1 -0.5 0.5)))
         (transfer-config (getf config :transfer)) ;may be a keyword or a function
         (transfer (if-typep-let (f transfer-config) #'functionp f (find-function f)))
         (derivative (if-typep-let (f (getf config :derivative)) #'functionp f (derivative transfer-config)))
         (network-input-config (getf config :network-input))
         (network-input-weights (make-weights-from-config network-input-config)) ;initialize default weights when provided
         (network-inputs (make-network-inputs-from-config network-input-config))
         (layer-weights-config (getf config :layer-input))
         (layer-weights (make-weights-from-config layer-weights-config)) ;initialize default weights when provided
         (layer-inputs (make-layer-inputs-from-config layer-weights-config))
         (link-forward (loop for (id nil) in layer-weights collect id))
         (link-to (getf config :link-to)))
    (make-instance 'lddn-layer
                 :id id :neurons neurons :bias bias :transfer transfer :derivative-fun derivative
                 :link-to link-to
                 :network-input-weights network-input-weights
                 :network-inputs network-inputs
                 :layer-weights layer-weights
                 :layer-inputs layer-inputs
                 :link-forward link-forward
                 )))

(defun make-weights-from-config (weights-config)
  "make an associate list with id and tdls of weights, can be used to initialize network-input-weights and layer-weights
if weights are in the config, add them to the tdl, else initialize the tdl with nil value.
parameter: (list (list :id 1 :w (list '((1/3 1/3 /13) (1/3 1/3 1/3)))))"
  (when weights-config ;some weights, such as IWs or the 1st layer's LW, will not be avaliable, and should return nil
    (loop for plist in weights-config
          collect (list (getf plist :id)
                        (progn
                          (let ((tdl (make-delay-from-config (getf plist :delay) nil)))
                                                        (alexandria:when-let (weights (getf plist :w))
                              (dolist (w weights) (add-tdl-content tdl w))
                              (when (= (from tdl) 1) ;forward and from 1, see function make-delay-from-config
                                (add-tdl-content tdl nil)))
                            tdl))))))

(defun make-network-inputs-from-config (network-input-config)
  "see make-layer-weights-from-config, but initialize the tdls with 0 and do not need to provide a init value in the config"
  (loop for plist in network-input-config
        collect (list (getf plist :id)
                      (make-delay-from-config (getf plist :delay) 0))))

(defun make-layer-inputs-from-config (layer-input-config)
  "see make-layer-weights-from-config, but initialize the tdls with 0 and do not need to provide a init value in the config"
  (loop for plist in layer-input-config
        collect (list (getf plist :id)
                      (make-delay-from-config (getf plist :delay) 0))))

(defun gen-inputs-from-config (config)
  "return and associate list about each input's id and dimension, this function is used to initialize the inputs slot of lddn.
config: (list (list :id 1 :dimension 3 :to-layer '(1)))"
  (loop for cfg in config
        collect (list (getf cfg :id) (getf cfg :dimension))))

(defun gen-input-to-from-config (config)
    "return and associate list about each input's id and which layers it will input to, this function is used to initialize the input-to slot of lddn.
config: (list (list :id 1 :dimension 3 :to-layer '(1)))"
  (loop for cfg in config
        collect (list (getf cfg :id) (getf cfg :to-layer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; pretty print

(defmethod format-string ((layer lddn-layer))
  "return a format string about the object"
  (format nil "~&id: ~d, neurons: ~d, link-to: ~d, link-forward: ~d, link-backward: ~d~%bias: ~d~%network-inputs: ~d~%network-input-weights: ~d~%layer-inputs: ~d~%layer-weights: ~d"
            (id layer)
            (neurons layer)
            (format nil "(~{~d~^ ~})" (link-to layer))
            (format nil "(~{~d~^ ~})" (link-forward layer))
            (format nil "(~{~d~^ ~})" (link-backward layer))
            (alexandria:when-let (b (bias layer))
              (if (listp b) (format nil "(~{~,3f~^ ~})" (first (transpose b)))
                  (format nil "(~,3f)" b)))
            (alexandria:if-let (x (network-inputs layer)) (id-tdl-alist-format x) nil)
            (alexandria:if-let (x (network-input-weights layer)) (id-tdl-alist-format x) nil)
            (alexandria:if-let (x (layer-inputs layer)) (id-tdl-alist-format x) nil)
            (alexandria:if-let (x (layer-weights layer)) (id-tdl-alist-format x) nil)))

(defmethod print-object ((layer lddn-layer) stream)
  (print-unreadable-object (layer stream :type t)
    (format stream "~d" (format-string layer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get's and set's about the slots of an lddn-layer
(defmethod get-layer-id ((layer lddn-layer))
  (id layer))

(defmethod get-neurons ((layer lddn-layer))
  (neurons layer))

(defmethod get-layer-input ((layer lddn-layer) (input-layer-id integer))
  "get the layer input tdl"
  (with-slots ((li layer-inputs)) layer
    (second (assoc input-layer-id li))))

(defmethod get-layer-weight ((layer lddn-layer) (input-layer-id integer))
  "get the layer weight tdl"
  (with-slots ((lw layer-weights)) layer
    (second (assoc input-layer-id lw))))

(defmethod get-layer-network-input-weight ((layer lddn-layer) (l-th-input integer))
  "get the network input weight tdl"
  (with-slots ((niw network-input-weights)) layer
    (second (assoc l-th-input niw))))

(defmethod get-layer-bias ((layer lddn-layer))
  (bias layer))

(defmethod get-neuron-output ((layer lddn-layer))
  (neuron-output layer))

(defmethod set-neuron-output! ((layer lddn-layer) value)
  (setf (neuron-output layer) value))

(defmethod set-link-backward! ((layer lddn-layer) layer-id-list)
  (setf (link-backward layer) layer-id-list))

(defmethod get-link-forward ((layer lddn-layer))
  (link-forward layer))

(defmethod get-link-backward ((layer lddn-layer))
  (link-backward layer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;

(defmethod calc-layer-net-input ((layer lddn-layer))
  "calc the net output of layer m at time t, the equation is (14.1)"
  (matrix-multi-add ;will remove nil first
   (list
    ;;layer inputs part
    (with-slots ((lf link-forward)
                 (li layer-inputs)
                 (lw layer-weights)) layer
      (matrix-multi-add
       (loop for l in lf
             collect (matrix-multi-add
                      (loop for delay-input in (get-tdl-effective-content (second (assoc l li)))
                            for delay-weight in (get-tdl-effective-content (second (assoc l lw)))
                            collect (matrix-product delay-weight delay-input))))))
    ;;network inputs part
    (with-slots ((ni  network-inputs) ;may produce nil
                 (niw network-input-weights)) layer
      (matrix-multi-add
       (loop for (l delay-input) in ni ;ni is an associate list about input id and it's tdl
             collect (matrix-multi-add
                      (loop for each-delay-input in (get-tdl-effective-content delay-input)
                            for each-delay-weight in (get-tdl-effective-content (second (assoc l niw)))
                            collect (matrix-product each-delay-weight each-delay-input))))))
    (bias layer))))

(defmethod add-network-input-to-layer ((layer lddn-layer) raw-input-alist)
  "add the network's raw input to the network-input's tdl"
  (with-slots ((input network-inputs)) layer
    (loop for (lth-in tdl) in input
          do (alexandria:when-let (in-vec (assoc lth-in raw-input-alist))
               (add-tdl-content tdl (second in-vec))
               ))))

(defmethod layer-link-delay? ((layer-from lddn-layer) (layer-to lddn-layer))
  "check if the layer input from `layer-from to `layer-to has non-ZERO delay, before calling this function, one should make sure that there is a direct link from `layer-from to `layer-to"
  (let ((from-id (get-layer-id layer-from))
        (to-id (get-layer-id layer-to)))
    (if (null (find to-id (link-to layer-from)))
        (warn "layer ~d has no link to layer ~d" (get-layer-id layer-from) (get-layer-id layer-to)))
    (let ((tdl (get-layer-weight layer-to from-id)))
      (if (and (eq (get-tdl-type tdl) :forward) (= (tdl-fifo-length tdl) 1)) ;see make-delay-from-config for delay definition
          nil
          t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; class definition and initializing

(defclass lddn ()
  ((inputs :initarg :inputs :accessor inputs :type list :initform nil
           :documentation "associate list about each input's id and dimension")
   (input-to :initarg :input-to :accessor input-to :type list :initform nil
             :documentation "associate list about each input's id and the list of layers' id it will input to")
   (layers :initarg :layers :accessor layers :type list :initform nil
           :documentation "a associate list of the layers of the network, the key of the associate list is the layer's id")
   (input-layers :initarg :input-layers :accessor input-layers :type list :initform nil
                 :documentation "$X$ in the textbook, the list of layer id's of all the input layers")
   (output-layers :initarg :output-layers :accessor output-layers :type list :initform nil
                  :documentation "$U$ in the textbook, the list of layer id's of all the output layers")
   (raw-input-layers :initarg :raw-input-layers :accessor raw-input-layers :type list :initform nil
                         :documentation "the id of the layers that receive network's raw input")
   (final-output-layers :initarg :final-output-layers :accessor final-output-layers :type list :initform nil
                          :documentation "the id of the layers whose neuron-outputs will be used to compared with the target")
   (network-output :initarg :network-output :accessor network-output :type list :initform nil
                   :documentation "an associate list of output result of the lddn network, the keys in the associate list should be across the slot of network-output-layers")
   (simul-order :initarg :simul-order :accessor simul-order :type list :initform nil :documentation "simulation order")
   (bp-order :initarg :bp-order :accessor bp-order :type list :initform nil :documentation "backpropagation order")
   )
  (:documentation "Layered Digital Dynamic Network, the slot's names should reference to page 290, Chinese edition"))

(defun make-lddn (&key config)
  "return an lddn object, network-output slot will be initialized in :after, as well as some slots of the layers"
  ;;(make-lddn :config lddn-config-p14.1)
  (let* ((inputs (gen-inputs-from-config (getf config :input)))
         ;;it's not necessarily be configured since is can be reduced from the layers, the reason is to make a more clearer config
         (input-to (gen-input-to-from-config (getf config :input)))
         (layers (loop for layer-cfg in (getf config :layer)
                       collect (make-lddn-layer layer-cfg)))
         (raw-input-layers (remove-duplicates (loop for (id layer-ids) in input-to
                                                append layer-ids)))
         (final-output-layers (getf config :output))
         (simul-order (getf config :order))
         (bp-order (reverse simul-order)))
    (make-instance 'lddn :inputs inputs
                         :input-to input-to
                         :layers layers
                         :raw-input-layers raw-input-layers
                         :final-output-layers final-output-layers
                         :simul-order simul-order
                         :bp-order bp-order
                         :config config)
    ))

(defmethod initialize-instance :after ((lddn lddn) &key config  &allow-other-keys)
  "initialize network-output slot of lddn,
initizlize the layers' slots link-forward, link-backward, layer-weights, network-input-weights, bias"
  (with-slots ((layers layers)) lddn
    (dolist (layer layers)
      (with-slots ((IWs network-input-weights)
                   (LWs layer-weights)
                   (link-to link-to)
                   (layer-inputs layer-inputs)
                   (network-inputs network-inputs)) layer
        ;;randomize the IWs when they were not configured
        (loop for (input-id tdl) in IWs
              do (when (null (->> config ;see if the weight was configured
                                  #'(lambda (cfg) (getf cfg :layer))
                                  #'(lambda (cfg) (find (get-layer-id layer) cfg :key #'(lambda (x) (getf x :id))))
                                  #'(lambda (cfg) (getf cfg :network-input))
                                  #'(lambda (cfg) (find input-id cfg :key #'(lambda (x) (getf x :id))))
                                  #'(lambda (cfg) (getf cfg :w))))
                   (randomize-input-weights! lddn layer input-id)))
        ;;randomize the LWs when they were not configured
        (loop for (layer-id tdl) in LWs
              do (when (null (->> config ;see if the weight was configured
                                  #'(lambda (cfg) (getf cfg :layer))
                                  #'(lambda (cfg) (find (get-layer-id layer) cfg :key #'(lambda (x) (getf x :id))))
                                  #'(lambda (cfg) (getf cfg :layer-input))
                                  #'(lambda (cfg) (find layer-id cfg :key #'(lambda (x) (getf x :id))))
                                  #'(lambda (cfg) (getf cfg :w))))
                   (randomize-layer-weights! lddn layer layer-id)))
        ;;set link-backward for each layer
        (set-link-backward! layer (loop for id in link-to
                                        when (null (layer-link-delay? layer (get-layer lddn id)))
                                          collect id))
        ;;set default tdl content of layer-input to zero vector
        (loop for (layer-id tdl) in layer-inputs
              do (dotimes (i (tdl-fifo-length tdl))
                   (add-tdl-content tdl (make-zeros (get-neurons (get-layer lddn layer-id)) 1))))
        ;;set default tdl content of network-input to zero vector
        (loop for (input-id tdl) in network-inputs
              do (dotimes (i (tdl-fifo-length tdl))
                   (add-tdl-content tdl (make-zeros (get-input-dimension lddn input-id) 1))))
        ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; pretty print lddn object
(defmethod format-string ((lddn lddn))
  (with-slots ((inputs inputs)
               (input-to input-to)
               (layers layers)
               (input-layers input-layers)
               (output-layers output-layers)
               (raw-input-layers raw-input-layers)
               (final-output-layers final-output-layers)
               (simul-order simul-order)
               (bp-order bp-order)) lddn
  (format nil "Layers: ~d~%Inputs: ~d~%Input to layers: ~d~%Input layers(X): ~d~%Output layers(U): ~d~%Raw input layers: ~d~%Network output layers: ~d~%Simulation order: ~d~%Backpropagation order: ~d~%Layers:~&--------~&~d~&--------"
          (length simul-order)
          (format-string-inputs inputs)
          (format-string-input-to input-to)
          (format nil "~{~d~^ ~}" input-layers)
          (format nil "~{~d~^ ~}" output-layers)
          (format nil "~{~d~^ ~}" raw-input-layers)
          (format nil "~{~d~^ ~}" final-output-layers)
          (format nil "~{~d~^ ~}" simul-order)
          (format nil "~{~d~^ ~}" bp-order)
          (apply #'concatenate 'string
                 (list-interpolation
                  (loop for layer in layers
                        collect (format-string layer))
                  (concatenate 'string  (string #\newline) "----" (string #\newline)))))))

(defmethod format-string-inputs (input-list)
  (apply #'concatenate 'string
         (cons "Network inputs: "
               (list-interpolation
                (loop for (id dimension) in input-list
                      collect (format nil "<id: ~d, dimension: ~d>" id dimension))
                " "))))

(defmethod format-string-input-to (input-to-list)
  (apply #'concatenate 'string
         (cons "Network input to layers: "
               (list-interpolation
                (loop for (id to-list) in input-to-list
                      collect (format nil "<id: ~d, input to: ~{~d ~}>" id to-list))
                " "))))

(defmethod print-object ((lddn lddn) stream)
  (print-unreadable-object (lddn stream :type t)
    (format stream "~d" (format-string lddn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; get's and set's about lddn slots

(defmethod get-layer ((lddn lddn) layer-id)
  "get the layer instance whose is is `layer-id"
  (with-slots ((layers layers)) lddn
    (find layer-id layers :key #'get-layer-id)))

(defmethod get-input-dimension ((lddn lddn) input-id)
  (second (assoc input-id (inputs lddn))))

(defmethod get-layer-neurons ((lddn lddn) layer-id)
  "get the neurons num of the specified layer"
  (get-neurons (get-layer lddn layer-id)))

(defmethod get-input-layers ((lddn lddn))
  "get input-layers($X$) of this lddn network"
  (input-layers lddn))

(defmethod get-output-layers ((lddn lddn))
  "get output-layers($U$) of this lddn network"
  (output-layers lddn))

(defmethod get-simul-order ((lddn lddn))
  (simul-order lddn))

(defmethod get-bp-order ((lddn lddn))
  (bp-order lddn))

(defmethod get-input-layers ((lddn lddn))
  (input-layers lddn))

(defmethod get-output-layers ((lddn lddn))
  (output-layers lddn))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod randomize-input-weights! ((lddn lddn) (layer lddn-layer) input-id)
  "initialize the input weights as random matrices, this function is used as :after make-instance of lddn"
  (with-slots ((weights-alist network-input-weights)
               (neuron-num neurons)) layer
    (loop for (id tdl) in weights-alist
          do (with-slots ((c content)
                          (f from)) tdl
               (dotimes (i (- (tdl-fifo-length tdl) (from tdl)))
                 (add-tdl-content tdl (rand-matrix neuron-num (get-input-dimension lddn id) -0.5 0.5)))
               (dotimes (i (from tdl)) ;the content before `from will not be used and can be set to nil
                 (add-tdl-content tdl nil))))))

(defmethod randomize-layer-weights! ((lddn lddn) (layer lddn-layer) layer-id)
  "initialize the input weights as random matrices, this function is used as :after make-instance of lddn"
  (with-slots ((weights-alist layer-weights)
               (neuron-num neurons)) layer
    (loop for (id tdl) in weights-alist
          do (with-slots ((c content)
                          (f from)) tdl
               (dotimes (i (- (tdl-fifo-length tdl) (from tdl)))
                 (add-tdl-content tdl (rand-matrix neuron-num (get-layer-neurons lddn id) -0.5 0.5)))
               (dotimes (i (from tdl)) ;the content before `from will not be used and can be set to nil
                 (add-tdl-content tdl nil))))))

(defmethod calc-neuron-output! ((lddn lddn) (layer lddn-layer))
  "calc the output of this layer, and send the result to the layers it connects to"
  (let ((res (funcall (transfer layer) (calc-layer-net-input layer))))
    (set-neuron-output! layer res)
    (loop for send-to-id in (link-to layer)
          do (add-tdl-content (get-layer-input (get-layer lddn send-to-id) (get-layer-id layer))
                                     res))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod calc-lddn-output! ((lddn lddn) input-alist)
  "calc the output of the lddn network providing a list of input vectors"
  (with-slots ((layers layers)
               (input-layers raw-input-layers)
               (output-layers final-output-layers)
               (simul-order simul-order)) lddn
    (dolist (id input-layers) ;send input vector to the raw input layers
      (add-network-input-to-layer (get-layer lddn id) input-alist))
    (dolist (layer-id simul-order)
      (calc-neuron-output! lddn (get-layer lddn layer-id)))
    (loop for out-layer-id in output-layers ;collect network output result
          collect (list out-layer-id (get-neuron-output (get-layer lddn out-layer-id))))))


(defmethod calc-bptt-gradient ((lddn lddn) (samples list))
  "Backpropagation-Through-Time Gradient"
  (let* ((sample-num (length samples))
         (bp-order (get-bp-order lddn))
         (simul-order (get-simul-order lddn))
         (input-layers (get-input-layers lddn))
         (output-layers (get-output-layers lddn))
         (exist-sens-layer nil) ; $E_S(u)$, associate list which associate the layers that has non-zero sensitivities with the key
         (exist-sens-input-layer nil) ; $E_S^X(u)$, an alist like exist-sens, but only for the input layers' ids
         (partial-derivative)
         )
    (dotimes (i (1- sample-num) (format t "all samples were processed"))
      (let* ((output-layers-tmp nil)  ; $U^'=\phi$
             )
        (dolist (u output-layers)
          (when u (alist-push-or-replace! exist-sens-layer u nil))
          (when u (alist-push-or-replace! exist-sens-input-layer u nil)))
        (dolist (m bp-order) ;For m decremented through the BP order
          (let* ((layer-m (get-layer lddn m))
                 (link-backward-m (get-link-backward layer-m))
                 (F^m-n^m)
                 )
            (dolist (u output-layers-tmp)
              (alexandria:when-let (exi-sens-u (assoc u exist-sens-layer))
                (when (intersection (second exi-sens-u) link-backward-m) ;when the intersection is not empty
                  ;;calc $S^{u,m}$
                  ;;......
                  ;;......
                  (alist-push-or-replace! exist-sens-layer u m)
                  (when (member m input-layers)
                    (alist-adjoin-to-value-set! exist-sens-input-layer u m)))))
            (when (member m output-layers)
              ;;calc S^{m,m}=F
              (adjoin m output-layers-tmp)
              (alist-adjoin-to-value-set! exist-sens-layer m m)
              (when (member m input-layers)
                (alist-adjoin-to-value-set! exist-sens-input-layer m m)))))

        (dolist (m simul-order)

          )

        ))))


























;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; demos, examples, and exercises

(defparameter lddn-config-p14.1
  (list :input (list (list :id 1 :dimension 3 :to-layer '(1))) ;the first input vector, 3*1 rank, input to layer 1
        :output (list 10) ;network output layer id
        :order '(1 2 3 7 4 5 6 8 9 10)
        ;;transfer may refer the type or provide a function, if a function f was provided , the drivative should provide too
        :layer (list (list :id 1 :neurons 2 :transfer :logsig
                           ;;receive the first input with delay from 0, and the tdl has length 1
                           ;;:w is a list of weights and should correspond to each delay, if :w is not provided, the weights will be generated randomly
                           :network-input (list (list :id 1 :w (list '((1/3 1/3 1/3) (1/3 1/3 1/3)))))
                           :layer-input nil
                           :bias '((0) (0))
                           :link-to '(2))
                     (list :id 2 :neurons 3 :transfer :logsig
                           :layer-input (list (list :id 1 :w (list '((1/2 1/2) (1/2 1/2) (1/2 1/2))))
                                              (list :id 2 :delay (list :from 1 :to 1 :dir :self)
                                                    :w (list '((1/3 1/3 1/3) (1/3 1/3 1/3) (1/3 1/3 1/3)))))
                           :link-to '(2 3 6 7))
                     (list :id 3 :neurons 2 :transfer :logsig
                           :layer-input (list (list :id 2 :w (list '((1/3 1/3 1/3) (1/3 1/3 1/3)))))
                           :link-to '(4))
                     (list :id 4 :neurons 1 :transfer :logsig
                           :layer-input (list (list :id 3 :delay (list :from 1 :to 1 :dir :forward) :w (list '((1/2 1/2))))
                                              (list :id 4 :delay (list :from 1 :to 1 :dir :self) :w (list '((1))))
                                              (list :id 7 :w (list '((1/2 1/2)))))
                           :link-to '(4 5 9))
                     (list :id 5 :neurons 2 :transfer :logsig
                           :layer-input (list (list :id 4 :w (list '((1/2) (1/2)))))
                           :link-to '(6))
                     (list :id 6 :neurons 4 :transfer :logsig
                           :layer-input (list (list :id 5 :delay (list :from 0 :to 1 :dir :forward)
                                                    :w (list '((1/2 1/2) (1/2 1/2) (1/2 1/2) (1/2 1/2))
                                                             '((1/2 1/2) (1/2 1/2) (1/2 1/2) (1/2 1/2))))
                                              (list :id 2 :w (list '((1/3 1/3 1/3) (1/3 1/3 1/3) (1/3 1/3 1/3) (1/3 1/3 1/3)))))
                           :link-to '(10))
                     (list :id 7 :neurons 2 :transfer :logsig
                           :layer-input (list (list :id 2 :w (list '((1/3 1/3 1/3) (1/3 1/3 1/3)))))
                           :link-to '(4 8))
                     (list :id 8 :neurons 3 :transfer :logsig
                           :layer-input (list (list :id 7 :w (list '((1/2 1/2) (1/2 1/2) (1/2 1/2)))))
                           :link-to '(9))
                     (list :id 9 :neurons 2 :transfer :logsig
                           :layer-input (list (list :id 8 :delay (list :from 1 :to 1 :dir :forward)
                                                    :w (list '((1/3 1/3 1/3) (1/3 1/3 1/3))))
                                              (list :id 4 :w (list '((1) (1)))))
                           :link-to '(10))
                     (list :id 10 :neurons 1 :transfer :purelin
                           :layer-input (list (list :id 6 :w (list '((1/4 1/4 1/4 1/4))))
                                              (list :id 9 :w (list '((1/2 1/2)))))))))

(defparameter lddn-config-graph-14.2 ;page 272
  (list :input (list (list :id 1 :dimension 1 :to-layer '(1)))
        :output (list 1)
        :order '(1)
        :layer (list (list :id 1 :neurons 1 :transfer :purelin
                           :bias 0
                           :network-input (list (list :id 1 :delay (list :from 0 :to 2 :dir :forward)
                                                      :w (list 1/3 1/3 1/3)))))))

(defparameter lddn-config-graph-14.4 ;page 273
  (list :input (list (list :id 1 :dimension 1 :to-layer '(1)))
        :output (list 1)
        :order '(1)
        :layer (list (list :id 1 :neurons 1 :transfer :purelin
                           :bias 0
                           :network-input (list (list :id 1 :w (list 1/2)))
                           :layer-input (list (list :id 1 :delay (list :from 1 :to 1 :dir :self) :w (list 1/2)))
                           :link-to '(1)))))


(defparameter lddn-layer-config-p14.1
  (first (getf lddn-config-p14.1 :layer)))

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
               (add-tdl-content tdl (funcall square-wave))
               (setf network-output (funcall transfer
                                             (reduce #'matrix-add
                                                     (list (matrix-product input-weight (tdl-to-vector tdl))
                                                           bias))))
               (format t "~&~d~d~,3f~%" i #\tab network-output)))))

(defun FIR-demo-lddn ()
  "Finite Impulse Response Network Demonstration, using a lddn network"
  (let* ((lddn (make-lddn :config lddn-config-graph-14.2))
         (square-wave (square-wave-generator 1 10)))
    (loop for i from 0 to 19
          do (progn
               (let* ((p (funcall square-wave))
                      (network-input (list (list 1 p)))
                      (network-output (calc-lddn-output! lddn network-input)))
                 (format t "~&~d, input: ~d, output: ~,3f~%" i p (cadar network-output)))))))

(defun IIR-demo-lddn ()
  "Infinite Impulse Response Network Demonstration, using a lddn network"
  (let* ((lddn (make-lddn :config lddn-config-graph-14.4))
         (square-wave (square-wave-generator 1 10)))
        (loop for i from 0 to 19
          do (progn
               (let* ((p (funcall square-wave))
                      (network-input (list (list 1 p)))
                      (network-output (calc-lddn-output! lddn network-input)))
                     (format t "~&SERIES: ~d, input: ~d, output: ~,3f~%" i p (cadar network-output)))))))

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
               (add-tdl-content tdl network-output) ; make a delay
               (format t "~&~d~d~,3f~%" i #\tab network-output)))))

(defun test-output-p14.2 ()
  "page 291"
  (let* ((lddn (make-lddn :config lddn-config-p14.1))
         (p '((1 ((1) (1) (1)))))
         (output (calc-lddn-output! lddn p)))
    (format t "~&lddn's inputs: ~d~%" (inputs lddn))
    (format t "~&input-to: ~d~%" (input-to lddn))
    (format t "~&raw-input-layers: ~d~%" (raw-input-layers lddn))
    (format t "~&final-output-layers: ~d~%" (final-output-layers lddn))
    (format t "~d~%" output)))
