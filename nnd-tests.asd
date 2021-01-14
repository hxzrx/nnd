;;;; nnd-tests.asd

(defsystem #:nnd-tests
  :author "He Xiang-zhi <xz.he@qq.com>"
  :license "MulanPSL-2.0"
  :depends-on (#:nnd
               #:cl-syslog
               #:fiasco)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :nnd-tests
                                           '#:run-nnd-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "packages")
               (:file "suite")
               (:file "util-tests")
               (:file "linear-algebra-tests")))
               
