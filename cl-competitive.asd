(asdf:defsystem :cl-competitive
  :components ((:file "src/main")
               (:file "src/sub"))
  :in-order-to ((test-op (test-op :cl-competitive/tests))))

(asdf:defsystem :cl-competitive/tests
    :depends-on (:cl-competitive :fiveam)
    :components ((:file "tests/main")
                 (:file "tests/sub"))
    :perform (test-op (o c) (symbol-call :fiveam '#:run! :foobar)))
