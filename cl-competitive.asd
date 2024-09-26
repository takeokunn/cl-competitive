(asdf:defsystem :cl-competitive
    :components ((:file "src/sandbox")
                 (:file "src/lib/queue"))
    :in-order-to ((test-op (test-op :cl-competitive/tests))))

(asdf:defsystem :cl-competitive/tests
    :depends-on (:cl-competitive :fiveam)
    :components ((:file "tests/sandbox")
                 (:file "tests/lib/queue"))
    :perform (test-op (o c) (symbol-call :fiveam '#:run! :foobar)))
