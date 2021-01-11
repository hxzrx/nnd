(in-package #:nnd)

;;; hard limit
(defgeneric hardlim (net-output) 
  (:documentation "hard limit, return 1 when netoutput >= 0 else return 0"))
  
(defmethod hardlim ((net-output number))
  "for a singl real number"
  (declare (type real net-output))
  (if (< net-output 0) 0 1))

(defmethod hardlim ((net-output list))
  "hardlim for a column vector"
  (collect-transfer #'hardlim net-output))


;;;; symmetrical hard limit
(defgeneric hardlims (net-output)
  (:documentation "symmetrical hard limit, return 1 when netoutput >= 0 else return -1"))

(defmethod hardlims ((net-output number))
  "symmetrical hard limit"
  (declare (type real net-output))
  (if (< net-output 0) -1 1))

(defmethod hardlims ((net-output list))
  "hardlims for a column vector"
  (collect-transfer #'hardlims net-output))


;;;; purelin
(defgeneric purelin (net-output)
  (:documentation "return input itself"))

(defmethod purelin ((net-output number))
  "linear"
  (declare (type real net-output))
  net-output)

(defmethod purelin ((net-output list))
  "linear"
  (collect-transfer #'purelin net-output))


;;;; satlin
(defgeneric satlin (net-output)
  (:documentation "satlin"))

(defmethod satlin (n)
  "saturating linear"
  (declare (type real n))
  (if (< n 0) 0
      (if (> n 1) 1 n)))

(defmethod satlin ((net-output list))
  "list type"
  (collect-transfer #'satlin net-output))

;;;; satlins
(defgeneric satlins (net-output)
  (:documentation "satlins"))

(defmethod satlins ((n number))
  "symmertic saturating linear"
  (declare (type real n))
  (if (< n -1) -1
      (if (> n 1) 1 n)))

(defmethod satlins ((net-output list))
  "list type"
  (collect-transfer #'satlins net-output))

;;;; logsig
(defgeneric logsig (net-output)
  (:documentation "logsig"))

(defmethod logsig ((n number))
  "log-sigmoid"
  (declare (type real n))
  (/ 1 (+ 1 (exp (- n)))))

(defmethod logsig ((net-output list))
  "list type"
  (collect-transfer #'logsig net-output))


;;;; tansig
(defgeneric tansig (net-output)
  (:documentation "tansig"))

(defmethod tansig ((n number))
  "hyperbolic tangent sigmoid"
  (declare (type real n))
  (/ (- (exp n) (exp (- n))) (+ (exp n) (exp (- n)))))

(defmethod tansig ((net-output list))
  "list type"
  (collect-transfer #'tansig net-output))

;;;; poslin
(defgeneric poslin (net-output)
  (:documentation "poslin"))

(defmethod poslin ((n number))
  "positive linear"
  (declare (type real n))
  (if (< n 0) 0 n))

(defmethod poslin ((net-output list))
  "list type"
  (collect-transfer #'poslin net-output))

(defgeneric compet (neuron)
  (:documentation "competitive"))
