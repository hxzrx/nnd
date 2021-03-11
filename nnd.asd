;;;; nnd.asd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Common Lisp code for the answers in "Neural Network Design, 2nd, Martin T. Hagan"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem #:nnd
  :description "Common Lisp code for \"Neural Network Design, 2nd\", Martin T. Hagan.
                Including a linear algebra library from scratch."
  :author "He Xiang-zhi <xz.he@qq.com>"
  :maintainer "He Xiang-zhi <xz.he@qq.com>"
  :homepage "https://gitee.com/hxz/nnd"
  :version 0.1.2
  :license "MulanPSL-2.0"
  :depends-on (#:alexandria
               #:assoc-utils
               ;;#:bordeaux-threads       ; thread lib
               ;;#:cl-syslog              ; send logs to syslogd
               ;;#:magicl                 ; matrix algebra https://github.com/rigetti/magicl
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:nnd-tests)))
  :pathname "./"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "data-objects")
               (:file "linear-algebra")
               (:file "transfer-functions")
               (:file "learner")
               (:file "perception")
               (:file "performance")
               (:file "widrow-hoff")
               (:file "bp")
               (:file "bp-variations")
               (:file "generalization")
               (:file "dynamic")
               (:file "competitive")))
