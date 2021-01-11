;;;; nnd.asd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Common Lisp code for the answers in "Neural Network Design, 2nd, Martin T. Hagan"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem #:nnd
  :description "Common Lisp code for the answers in Neural Network Design, 2nd, Martin T. Hagan"
  :author "He Xiang-zhi <xz.he@qq.com>"
  :version 0.0.1
  :depends-on (#:bordeaux-threads       ; thread lib
               #:cl-syslog              ; send logs to syslogd
               #:magicl                 ; matrix algebra https://github.com/rigetti/magicl
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:nnd-tests)))
  :pathname "./"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "linear-algebra")
               (:file "transfer-functions")
               (:file "perception")
               ))

