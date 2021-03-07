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
   (deriv-F-n :initarg :deriv-F-n :accessor deriv-F-n :type list :initform nil
              :documentation "$\dot{F}^u(n^u(t))$, an diag matrix with diag elements the deriv of transfer to each element of the net input of this layer")
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
   (exist-lw-from-output :initarg :exist-lw-from-output :accessor exist-lw-from-output :type list :initform nil
                         :documentation "$E_{LW}^U$, the ids of the output layers that connect to this layer (which will always be an input layer) with at least some nonzero delay")
   (exist-lw-from-input :initarg :exist-lw-from-input :accessor exist-lw-from-input :type list :initform nil
                        :documentation "$E_{LW}^X$, the ids of the input layers that have a connection from this layer with at least some nonzero delay")
   (exist-sens-input-layer :initarg :exist-sens-input-layer :accessor exist-sens-input-layer :type list :initform nil
                           :documentation "$E_S^X$, a list of input layers that has non-zero sensitivities to this layer, generated when calculating gradient")
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
         (link-to (getf config :link-to))
         (exist-lw-from-output (make-exist-lw-from-output layer-weights))
         )
    (make-instance 'lddn-layer
                 :id id :neurons neurons :bias bias :transfer transfer :derivative-fun derivative
                 :link-to link-to
                 :network-input-weights network-input-weights
                 :network-inputs network-inputs
                 :layer-weights layer-weights
                 :layer-inputs layer-inputs
                 :link-forward link-forward
                 :exist-lw-from-output exist-lw-from-output
                 )))

(defun make-exist-lw-from-output (layer-weight)
  "initialize slot exist-lw-from-output, the parameter is layer-weight"
  (loop for (id tdl) in layer-weight
        when (tdl-has-delay? tdl)
        collect id))

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
  (format nil "~&id: ~d, neurons: ~d, link-to: ~d, link-forward: ~d, link-backward: ~d~%Exists LW from U, E_LW^U: ~d~%Esists LW from X, E_LW^X: ~d~%bias: ~d~%network-inputs: ~d~%network-input-weights: ~d~%layer-inputs: ~d~%layer-weights: ~d"
            (id layer)
            (neurons layer)
            (format nil "(~{~d~^ ~})" (link-to layer))
            (format nil "(~{~d~^ ~})" (link-forward layer))
            (format nil "(~{~d~^ ~})" (link-backward layer))
            (format nil "(~{~d~^ ~})" (exist-lw-from-output layer))
            (format nil "(~{~d~^ ~})" (exist-lw-from-input layer))
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

(defmethod get-network-inputs ((layer lddn-layer))
  (network-inputs layer))

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

(defmethod get-link-to ((layer lddn-layer))
  (link-to layer))

(defmethod set-link-backward! ((layer lddn-layer) layer-id-list)
  (setf (link-backward layer) layer-id-list))

(defmethod get-link-forward ((layer lddn-layer))
  (link-forward layer))

(defmethod get-link-backward ((layer lddn-layer))
  (link-backward layer))

(defmethod get-net-input ((layer lddn-layer))
  (net-input layer))

(defmethod set-net-input! ((layer lddn-layer) value)
  (with-slots ((net-input net-input)) layer
    (setf net-input value)))

(defmethod get-deriv-F-n ((layer lddn-layer))
  (deriv-F-n layer))

(defmethod calc-deriv-F-n! ((layer lddn-layer))
  "should call when the net-input is ready. side effect: will modify deriv-F-n slot of `layer"
  (with-slots ((deriv derivative-fun)
               (net-input net-input)
               (deriv-F-n deriv-F-n)) layer
    (setf deriv-F-n (derivative-diag deriv (if (numberp net-input) net-input (first (transpose net-input)))))))

(defmethod set-deriv-F-n! ((layer lddn-layer))
  "should call when the net-input is ready. side effect: will modify deriv-F-n slot of `layer"
  (calc-deriv-f-n! layer))

(defmethod get-exist-lw-from-output ((layer lddn-layer))
  (exist-lw-from-output layer))

(defmethod get-exist-lw-from-input ((layer lddn-layer))
  (exist-lw-from-input layer))

;; this shoud be impled by a method of tdl
#+:ignore
(defmethod query-iw-delay-nums ((layer lddn-layer) input-id)
  "return a list of integers of the delay numbers about the input-id in this layer"
  (with-slots ((iw-alist network-input-weights)) layer ;an alist of id's and tdl's
    (let ((tdl (second (assoc input-id iw-alist))))
      (loop for i from (get-tdl-from tdl) below (tdl-fifo-length tdl)
            collect i))))

(defmethod query-iw-nth-delay ((layer lddn-layer) input-id delay)
  "return the iw from `input-id' whose delay is `delay', $IW^{this-layer-id,input-id}(delay)$"
  (with-slots ((iw-alist network-input-weights)) layer ;an alist of id's and tdl's
    (let ((tdl (second (assoc input-id iw-alist))))
      (query-tdl-content-by-delay tdl delay))))

(defmethod query-lw-nth-delay ((layer lddn-layer) input-layer-id delay)
  "return the iw from `layer-id' whose delay is `delay', $LW^{this-layer-id,input-layer-id}(delay)$"
  (with-slots ((lw-alist layer-weights)) layer ;an alist of id's and tdl's
    (let ((tdl (second (assoc input-layer-id lw-alist))))
      (query-tdl-content-by-delay tdl delay))))

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

(defmethod add-network-input-to-layer ((layer lddn-layer) raw-input-plist)
  "add the network's raw inputs to the network-input's tdl"
  (with-slots ((input network-inputs)) layer
    (loop for (lth-in tdl) in input
          do (alexandria:when-let (in-vec (getf raw-input-plist lth-in)) ;(in-vec (assoc lth-in raw-input-alist))
               (add-tdl-content tdl in-vec)
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

(defmethod layer-parameters ((layer lddn-layer))
  "by the dict order: network input weight > layer input weight > bias, sort layer id by #'< , sort delay from :from to the end of tdl"
  (with-slots ((IWs network-input-weights)
               (LWs layer-weights)
               (bias bias)) layer
    (apply #'append
           (remove nil (list
                        (when IWs (loop for (id tdl) in (sort IWs #'< :key #'first) ;a list of weights
                                        append (get-tdl-effective-content tdl)))
                        (when LWs (loop for (id tdl) in (sort LWs #'< :key #'first) ;a list of weights
                                        append (get-tdl-effective-content tdl)))
                        (list bias))))))

(defmethod layer-parameters-vector ((layer lddn-layer))
  "collect all the parameter of the layer to a column vector"
  ;;(layer-parameters-vector layer1)
  (loop for parameter in (layer-parameters layer)
        append (if (numberp parameter) (list (list parameter))
                   (matrix-to-vector parameter))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; lddn class definition and initializing

(defclass lddn ()
  ((inputs :initarg :inputs :accessor inputs :type list :initform nil
           :documentation "associate list about each input's id and dimension")
   (input-to :initarg :input-to :accessor input-to :type list :initform nil
             :documentation "associate list about each input's id and the list of layers' id it will input to")
   (layers :initarg :layers :accessor layers :type list :initform nil
           :documentation "a associate list of the layers of the network, the key of the associate list is the layer's id")
   (input-layers :initarg :input-layers :accessor input-layers :type list :initform nil
                 :documentation "$X$ in the textbook, the list of layer id's of all the input layers")
   (network-input-cache :initarg :network-input-cache :accessor network-input-cache :type list :initform nil
                        :documentation "an associate list about input id's and fixed length fifo's to cache each input vector")
   (network-output-cache :initarg network-output-cache :accessor network-output-cache :type list :initform nil
                         :documentation "an associate list about output layer id's and fixed length fifo's to cache each output vector")
   (output-layers :initarg :output-layers :accessor output-layers :type list :initform nil
                  :documentation "$U$ in the textbook, the list of layer id's of all the output layers")
   (raw-input-layers :initarg :raw-input-layers :accessor raw-input-layers :type list :initform nil
                         :documentation "the id of the layers that receive network's raw input")
   (final-output-layers :initarg :final-output-layers :accessor final-output-layers :type list :initform nil
                          :documentation "the id of the layers whose neuron-outputs will be used to compared with the target")
   (network-output :initarg :network-output :accessor network-output :type list :initform nil
                   :documentation "an associate list of output result of the lddn network, the keys in the associate list should be across the slot of network-output-layers")
   (max-network-input-delay :initarg :max-network-input-delay :accessor max-network-input-delay :type integer :initform 0
                            :documentation "associate list about the input ids and the max delays for each network input, used in (14.42)")
   (max-layer-input-delay :initarg :max-layer-input-delay :accessor max-layer-input-delay :type integer :initform 0
                          :documentation "an associate list about the layer ids and the max delays for each layer input, used in (14.43)")
   (simul-order :initarg :simul-order :accessor simul-order :type list :initform nil :documentation "simulation order")
   (bp-order :initarg :bp-order :accessor bp-order :type list :initform nil :documentation "backpropagation order")
   (sens-matrix-db :initarg :sens-matrix-db :accessor sens-matrix-db :type tabular-db
                   :documentation "sensitivity matrices")
   (F/a-deriv-exp-db :initarg :F/a-deriv-exp-db :accessor F/a-deriv-exp-db :type tabular-db
                     :documentation "explicit partial derivatives of performance function to the output of the output layers")
   (F/x-deriv-db :initarg :F/x-deriv-db :accessor F/x-deriv-db :type tabular-db
                 :documentation "partial derivatives of performance function to network parameters")
   (a/x-deriv-exp-db :initarg :a/x-deriv-exp-db :accessor a/x-deriv-exp-db :type tabular-db
                 :documentation "explicit partial derivatives of output of the output layers to network parameters")
   (a/x-deriv-db :initarg :a/x-deriv-db :accessor a/x-deriv-db :type tabular-db
                 :documentation "partial derivatives of output of the output layers to network parameters")
   (E-S-X-alist :initarg :E-S-X-alist :accessor E-S-X-alist :type list :initform nil
                :documentation "$E_S^X$, alist of E-S-X associated with layer id")
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
         (bp-order (reverse simul-order))
         (sens-matrix-db (make-tabular-db (list :to :from :value))) ;S^{:to,:from}
         (F/a-deriv-exp-db (make-tabular-db (list :output-layer :time :value))) ;∂Fᵉ/∂aᵘ for all u,  here, F is SSE
         (a/x-deriv-db (make-tabular-db (list :layer :time :param-type :to :from :delay :value))) ;param-type is {:lw :iw :b}
         (a/x-deriv-exp-db (make-tabular-db (list :layer :time :param-type :to :from :delay :value))) ;param-type is {:lw :iw :b}
         (F/x-deriv-db (make-tabular-db (list :layer :param-type :delay :value)))
         )
    (make-instance 'lddn :inputs inputs
                         :input-to input-to
                         :layers layers
                         :raw-input-layers raw-input-layers
                         :final-output-layers final-output-layers
                         :simul-order simul-order
                         :bp-order bp-order
                         :sens-matrix-db sens-matrix-db
                         :F/a-deriv-exp-db F/a-deriv-exp-db
                         :a/x-deriv-db a/x-deriv-db
                         :a/x-deriv-exp-db a/x-deriv-exp-db
                         :F/x-deriv-db F/x-deriv-db
                         :config config)
    ))

(defmethod initialize-instance :after ((lddn lddn) &key config  &allow-other-keys)
  "initialize network-output slot of lddn,
initizlize the layers' slots link-forward, link-backward, layer-weights, network-input-weights, bias"
  (with-slots ((simul-order simul-order)
               (layers layers)
               (inputs inputs)
               (input-to input-to)
               (raw-input-layers raw-input-layers)
               (U-list output-layers)
               (input-layers input-layers)
               (output-layers output-layers)
               (final-output-layers final-output-layers)
               (max-network-input-delay max-network-input-delay)
               (max-layer-input-delay max-layer-input-delay)
               (network-input-cache network-input-cache)
               (network-output-cache network-output-cache)) lddn
    ;; initialize the parts in the layers
    (dolist (layer layers)
      (with-slots ((IWs network-input-weights)
                   (LWs layer-weights)
                   (link-to link-to)
                   (exist-lw-from-input exist-lw-from-input)
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
        ;;set exist-lw-from-input for each layer
        (setf exist-lw-from-input  (loop for to-id in link-to
                                         when (layer-link-delay? layer (get-layer lddn to-id))
                                           collect to-id))
        ))

    ;;below will initialize the slots of lddn
    (setf U-list (reduce #'union (cons final-output-layers
                                       (loop for layer in layers
                                             collect (get-exist-lw-from-output layer)))))
    (setf input-layers (reduce #'union (cons raw-input-layers
                                             (loop for layer in layers
                                                   collect (get-exist-lw-from-input layer)))))
    (setf max-network-input-delay
          (loop for (input-id dim) in inputs
                collect (list input-id
                              (apply #'max
                                     (loop for input-to-layer-id in (second (assoc input-id input-to))
                                           collect (tdl-fifo-length
                                                    (second (assoc input-id (get-network-inputs
                                                                             (get-layer lddn input-to-layer-id))))))))))
    (setf max-layer-input-delay
          ;;(loop for layer-id in output-layers ;0307 00:18
          (loop for layer-id in simul-order
                collect (list layer-id
                              (apply #'max
                                     (alexandria:if-let
                                         (len (loop for to-layer-id in (get-link-to (get-layer lddn layer-id))
                                                    collect (tdl-fifo-length
                                                             (get-layer-input (get-layer lddn to-layer-id) layer-id))))
                                       len (list 1))))))
    ;; NEXT, define fixed-fifo to cache network inputs and layer inputs
    (setf network-input-cache
          (loop for (id delay) in max-network-input-delay
                collect (list id (make-fixed-len-unsafe-fifo
                                  delay
                                  :content (make-zeros (get-input-dimension lddn id) 1)))))
    (setf network-output-cache
          (loop for (id delay) in max-layer-input-delay
                collect (list id (make-fixed-len-unsafe-fifo
                                  delay
                                  :content (make-zeros (get-neurons (get-layer lddn id)) 1)))))

    ))

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
               (max-network-input-delay max-network-input-delay)
               (max-layer-input-delay max-layer-input-delay)
               (bp-order bp-order)) lddn
  (format nil "Layers: ~d~%Inputs: ~d~%Input to layers: ~d~%Input layers(X): ~d~%Output layers(U): ~d~%Raw input layers: ~d~%Network output layers: ~d~%Simulation order: ~d~%Backpropagation order: ~d~%Max network input delay: ~d~%Max layer input delay: ~d~%Layers:~&--------~&~d~&--------"
          (length simul-order)
          (format-string-inputs inputs)
          (format-string-input-to input-to)
          (format nil "~{~d~^ ~}" input-layers)
          (format nil "~{~d~^ ~}" output-layers)
          (format nil "~{~d~^ ~}" raw-input-layers)
          (format nil "~{~d~^ ~}" final-output-layers)
          (format nil "~{~d~^ ~}" simul-order)
          (format nil "~{~d~^ ~}" bp-order)
          (format nil "~d" max-network-input-delay)
          (format nil "~d" max-layer-input-delay)
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

;;RULE: function names use get-xxxx when fetching raw slot, and use query-xxxx when fetching processed data of the slots
(defmethod query-layer-weight ((lddn lddn) layer-id-from layer-id-to delay)
  "delay=0 means no delay only in :forward type, in other cases delay=0 means 1 delay"
  (with-slots ((layer-weights layer-weights)) (get-layer lddn layer-id-to)
    (let* ((tdl (second (assoc layer-id-from layer-weights)))
           (content (get-tdl-fifo-content tdl)))
      (with-slots ((from from)
                   (tdl-type tdl-type)) tdl
        (cond ((eq tdl-type :foreward) (nth delay content))
              (t (nth (1- delay) content)))))))

(defmethod query-network-input ((lddn lddn) input-id delay)
  "get the input vector whose id is `input-id' and delay is `delay'"
  (with-slots ((input-alist network-input-cache)) lddn ;the cache is a fixed length fifo
    (get-nth-content (second (assoc input-id input-alist)) delay)))

(defmethod query-network-output ((lddn lddn) output-id delay)
  "get the output vector whose id is `output-id' and delay is `delay'"
  (with-slots ((output-alist network-output-cache)) lddn ;the cache is a fixed length fifo
    (get-nth-content (second (assoc output-id output-alist)) delay)))

(defmethod query-E-S-X ((lddn lddn) layer-id)
  "get $E_S_X(u)$"
  (exist-sens-input-layer (get-layer lddn layer-id)))

(defmethod query-E-LW-U ((lddn lddn) layer-id)
  "get $E_{LW}^U(u)$"
  (exist-lw-from-output (get-layer lddn layer-id)))

(defmethod query-delay-link ((lddn lddn) to-layer-id from-layer-id)
  "get $DL_{to,from}$"
  (with-slots ((lw-alist layer-weights)) (get-layer lddn to-layer-id)
    (query-tdl-delays (second (assoc from-layer-id lw-alist)))))

#+:ignore
(defmethod set-exist-sens-input-layer ((lddn lddn) layer-id value)
  (with-slots ((esx exist-sens-input-layer)) (get-layer lddn layer-id)
    (setf esx value)))

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
  "calc the output of this layer, and send the result to the layers it connects to.
Side effect: will modify net-input slot of `layer, will modify neuron-output slot of `layer, will send the result to the layers it connect to, will modify deriv-F-n slot"
  (let* ((net-input (calc-layer-net-input layer))
         (res (funcall (transfer layer) net-input)))
    (set-net-input! layer net-input)
    (set-neuron-output! layer res)
    (add-network-output-to-cache! lddn (get-layer-id layer) res)
    (calc-deriv-F-n! layer)
    (loop for send-to-id in (link-to layer)
          do (add-tdl-content (get-layer-input (get-layer lddn send-to-id) (get-layer-id layer))
                              res))
    res
    ))

(defmethod add-network-input-to-cache! ((lddn lddn) raw-input-plist)
  "add the network's raw inputs to the network-input-cache slot of lddn"
  (with-slots ((fifo-alist network-input-cache)) lddn
    (loop for (id . input-vector) in (alexandria:plist-alist (first raw-input-plist))
          do (add-fixed-fifo (second (assoc id fifo-alist)) input-vector))))

(defmethod add-network-output-to-cache! ((lddn lddn) output-layer-id output-vector)
  "add the neuron output of some layer to the slot of network-output-cache of lddn"
  (with-slots ((fifo-alist network-output-cache)) lddn
    (alexandria:when-let (id-fifo (assoc output-layer-id fifo-alist))
    (add-fixed-fifo (second id-fifo) output-vector))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod calc-lddn-output! ((lddn lddn) input-plist)
  "calc the output of the lddn network providing a list of input vectors"
  (with-slots ((layers layers)
               (input-layers raw-input-layers)
               (output-layers final-output-layers)
               (simul-order simul-order)) lddn
    (dolist (id input-layers) ;send input vector to the raw input layers
      (add-network-input-to-layer (get-layer lddn id) input-plist)
      (add-network-input-to-cache! lddn input-plist))
    (dolist (layer-id simul-order)
      (calc-neuron-output! lddn (get-layer lddn layer-id)))
    (loop for out-layer-id in output-layers ;collect network output result
          collect (list out-layer-id (get-neuron-output (get-layer lddn out-layer-id))))))

(defmethod collect-init-sens-matrix-alist ((lddn lddn))
  "collect the sens matrix $S^{u,u}$ for all layer, associate them with (list u u)"
  (with-slots ((layers layers)
               (sens-tdb sens-matrix-db)) lddn
    (loop for layer in layers
          collect (list (list (get-layer-id layer) (get-layer-id layer))
                        (list (get-deriv-F-n layer))))))

(defmethod calc-init-sens-insert-db! ((lddn lddn))
  "calc sensitive matrix $S^{u,u}$ for all layer and insert into the db of the slot"
  (with-slots ((layers layers)
               (sens-tdb sens-matrix-db)) lddn
    (loop for layer in layers
          do (insert-tabular-db! sens-tdb (list :to (get-layer-id layer) :from (get-layer-id layer)
                                                :value (get-deriv-F-n layer))))))

(defmethod deriv-neoru-output-to-iw ((lddn lddn)  output-layer-u input-layer-m inter-layer-l)
  "equation (14.42), explicit partial derivative of neuro output of layer u to "

  )

(defmethod calc-bptt-gradient ((lddn lddn) (samples list))
  "Backpropagation-Through-Time Gradient"
  (let* ((time-step 0))
    (with-slots ((bp-order bp-order)
                 (simul-order simul-order)
                 (input-to input-to)
                 (input-layers input-layers)
                 (network-input-cache network-input-cache)
                 (network-output-cache network-output-cache)
                 (raw-input-layers raw-input-layers)
                 (output-layers output-layers)
                 (final-output-layers final-output-layers)
                 (sens-db sens-matrix-db)
                 (F/a-db F/a-deriv-exp-db)
                 (a/x-exp-db a/x-deriv-exp-db)
                 (a/x-deriv-db a/x-deriv-db)
                 (F/x-deriv-db F/x-deriv-db) ;the gradient to be returned
                 (E-S-X E-S-X-alist)
                 ) lddn
      (dolist (sample samples)
        (incf time-step)
        ;; some db's should be truncated in the beginning of each sample
        (truncate-tabular-db! sens-db)
        (truncate-tabular-db! F/a-db)
        (truncate-tabular-db! a/x-exp-db)

        (calc-lddn-output! lddn sample) ;forward propagation to make an output and get the intermediate results
        (calc-init-sens-insert-db! lddn) ;calc $S^{u,u}$ for all u

        (let* ((target (second sample))
               (output-layers-tmp nil)  ; $U'$
               (exist-sens-layer nil) ;$E_S(u)$, an alist which associate the layers that has non-zero sensitivities with the key
               (exist-sens-input-layer nil) ; $E_S^X(u)$, an alist like exist-sens, but only for the input layers' ids
               )
          ;; calc ∂Fᵉ/∂aᵘ for all u,  here, F is SSE
          (loop for u in output-layers
                do (insert-tabular-db! F/a-db (list :output-layer u :time time-step
                                                    :value (partial-deriv-SSE (if (member u final-output-layers)
                                                                                  (getf target u)
                                                                                  nil)
                                                                              (get-neuron-output (get-layer lddn u))
                                                                              (if (member u final-output-layers) t nil)))))
          #+:ignore
          (dolist (u output-layers) ;this dolist seems not necessarily since they are default nil if not specified
            (assert (member u simul-order)) ;make sure u is not nil, will remove this line later
            (setf exist-sens-layer (alist-create-or-adjoin exist-sens-layer u nil)) ; $E_S(u) = \Phi$
            (setf exist-sens-input-layer (alist-create-or-adjoin exist-sens-input-layer u nil))) ; $E_S^X(u)=\Phi$

          (dolist (m bp-order) ;for m decremented through the BP order
            (format t "~&Loop for backpropagation order: ~d~%" m)
            (let* ((layer-m (get-layer lddn m))
                   (link-backward-m (get-link-backward layer-m))) ;$L_m^b$
              (dolist (u output-layers-tmp)
                (alexandria:when-let (exi-sens-u (second (assoc u exist-sens-layer)))
                  (alexandria:when-let (intersect (intersection exi-sens-u link-backward-m))
                    ;;calc $S^{u,m}$, equation (14.38)
                    (let ((sens-matrix-u-m
                            (matrix-product
                             (reduce #'matrix-add
                                     (loop for l in intersect
                                           collect
                                           (matrix-product
                                            (query-tabular-db-value sens-db (list :to u :from l) :value)
                                            (first (get-tdl-effective-content
                                                    (get-layer-weight (get-layer lddn l) m))))))
                             (query-tabular-db-value sens-db (list :to m :from m) :value)
                             )))
                      (insert-tabular-db! sens-db (list :to u :from m :value sens-matrix-u-m)))

                    (setf exist-sens-layer (alist-create-or-adjoin exist-sens-layer u m))
                    (when (member m input-layers)
                      (setf exist-sens-input-layer (alist-create-or-adjoin exist-sens-input-layer u m)))
                    )))
              (when (member m output-layers)
                ;;the calculating of $S^{m,m}(t)=\dot{F}^m(n^m(t))$ was batch set after calc-lddn-output!
                (setf output-layers-tmp (adjoin m output-layers-tmp))
                (setf exist-sens-layer (alist-create-or-adjoin exist-sens-layer m m))
                (when (member m input-layers)
                  (setf exist-sens-input-layer (alist-create-or-adjoin exist-sens-input-layer m m))))
              )) ;end for dolist m

          (setf E-S-X exist-sens-input-layer) ;save to the slot of lddn

          (dolist (u simul-order)
            (format t "~&Loop for simulation order: ~d~%" u)
            (when (member u output-layers)
              ;;calc ∂ᵉaᵘ(t)/∂vec(IWᵐˡ(d))ᵀ
              (loop for l in raw-input-layers
                    do (loop for m in (second (assoc l input-to))
                             do (progn
                                  (format t "calc ∂ᵉaᵘ(t)/∂vec(IWᵐˡ) for u=~d, m=~d, l=~d~%" u m l)
                                  (loop for delay in (query-tdl-delays (get-layer-network-input-weight (get-layer lddn m) l))
                                        do (progn
                                             (alexandria:when-let (sens (query-tabular-db-value sens-db
                                                                                                (list :to u :from m) :value))
                                               (insert-tabular-db!
                                                a/x-exp-db
                                                (list :layer u
                                                      :time time-step
                                                      :param-type :iw
                                                      :to m
                                                      :from l
                                                      :delay delay
                                                      :value  (kroncker-product
                                                               (get-nth-content (second (assoc l network-input-cache)) delay)
                                                               sens))))
                                             )))))

              ;;calc ∂ᵉaᵘ(t)/∂vec(LWᵐˡ(d))ᵀ
              (loop for l in input-layers
                    do (loop for m in (get-link-to (get-layer lddn l))
                             do (progn
                                  (format t "calc ∂ᵉaᵘ(t)/∂vec(LWᵐˡ) for u=~d, m=~d, l=~d~%" u m l)
                                  (loop for delay in (query-tdl-delays (get-layer-weight (get-layer lddn m) l))
                                        for delay-type = (get-tdl-type (get-layer-weight (get-layer lddn m) l))
                                        do (progn
                                             (alexandria:when-let (sens (query-tabular-db-value sens-db
                                                                                                (list :to u :from m) :value))
                                               (insert-tabular-db!
                                                a/x-exp-db
                                                (list :layer u
                                                      :time time-step
                                                      :param-type :lw
                                                      :to m
                                                      :from l
                                                      :delay delay
                                                      :value  (kroncker-product
                                                               (get-nth-content (second (assoc l network-output-cache))
                                                                                (- delay (query-tdl-delay-base
                                                                                          (get-layer-weight (get-layer lddn m) l))))
                                                               sens))))
                                             )))))

              ;;calc ∂ᵉaᵘ(t)/∂(bᵐ)ᵀ, need only 2 parameters: u and m, so we set both :to and :from to m
              (loop for m in bp-order
                    do (progn
                         (format t "calc ∂ᵉaᵘ(t)/∂(bᵐ)ᵀ: u=~d, m=~d~%" u m)
                         ;; query not in sens-db will be treated as not a explicit partial derivative
                         (alexandria:when-let (sens (query-tabular-db-value sens-db (list :to u :from m) :value))
                                    (insert-tabular-db! a/x-exp-db
                                                        (list :layer u
                                                              :time time-step
                                                              :param-type :b
                                                              :to m
                                                              :from m
                                                              :value sens)))))

              ;; calc ∂a(t)/∂xᵀ here, a/x-deriv-db (list :layer :time :param-type :to :from :delay :value)
              (loop for m in simul-order ;for each layer
                    do (progn
                         (with-slots ((iws-alist network-input-weights)
                                      (lw-alist layer-weights)
                                      (bias bias)) (get-layer lddn m)
                           ;; ∂aᵘ(t)/∂vec(IWᵐˡ(d))ᵀ for all m and all l and all d
                           ;;a/x-exp-db (list :layer :time :param-type :to :from :delay :value)
                           ;;sens-db (list :to :from :value)
                           (loop for x in (query-E-S-X lddn u)
                                 when (query-tabular-db-value sens-db (list :to u :from x) :value)
                                   collect
                                   (matrix-product (query-tabular-db-value sens-db (list :to u :from x) :value)
                                                   (reduce #'matrix-add
                                                           (loop for u-tmp in (query-E-LW-U lddn x)
                                                                 collect
                                                                 (loop for d in (query-delay-link lddn x u-tmp)
                                                                       when (> (- time-step d) 0)
                                                                        collect
                                                                        (matrix-product
                                                                         (query-lw-nth-delay (get-layer lddn x) u-tmp d)
                                                                         (query-tabular-db-value
                                                                          a/x-deriv-db
                                                                          (list :layer u
                                                                                :time (- time-step d)
                                                                                :param-type :iw
                                                                                :to x
                                                                                :from u-tmp
                                                                                :delay d)
                                                                          :value)))))))





                           ;; ∂aᵘ(t)/∂vec(LWᵐˡ(d))ᵀ for all m and all l and all d
                           ;;....

                           ;; ∂aᵘ(t)/∂(bᵐ)ᵀ for all m
                           ;;....
                         )))
              ));end for dolist (u simul-order)

          ;; accumulate ∂F/∂x here
          ;;....
          )
        ) ;end for dolist (sample samples)
      F/x-deriv-db ;return tabular-db of ∂F/∂x
      )))

























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
         (p '((1 ((1) (1) (1)) 1)))
         (output (calc-lddn-output! lddn p)))
    (format t "~&lddn's inputs: ~d~%" (inputs lddn))
    (format t "~&input-to: ~d~%" (input-to lddn))
    (format t "~&raw-input-layers: ~d~%" (raw-input-layers lddn))
    (format t "~&output-layers: ~d~%" (output-layers lddn))
    (format t "~&final-output-layers: ~d~%" (final-output-layers lddn))
    (format t "~&sens matrices alist: ~{~d~%~}~%" (collect-init-sens-matrix-alist lddn))
    (format t "~d~%" output)))


(defun test-calc-bptt-gradient-p14.1 ()
  "each sample is a list of input and outputs, and it has this format:
(list (list  input-id1 input-vector1 input-id2 input-vector2 ...)
      (list output-id1 target-vector1 output-id2 target-vector2 ...))
all samples are a list of samples,
for the case of multiple targets (more than one output to compare with the targets), `target will be a list,
for simplicity, we assume that where's only one target.
"
  (let* ((lddn (make-lddn :config lddn-config-p14.1))
         (samples (list '((1 ((1) (1) (1))) (10 1)) )))
    (format t "~&All samples: ~d~%" samples)
    (calc-bptt-gradient lddn samples)))
