;;;; nnd-tests.asd

(asdf:defsystem #:nnd-tests
  :description "tests for nnd."
  :author "He Xiang-zhi <xz.he@qq.com>"
  :depends-on (
               #:nnd
               #:cl-syslog
               #:fiasco
               )
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :nnd-tests
                                           '#:run-nnd-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "packages")
               (:file "suite")
               (:file "util-tests")
               ))
