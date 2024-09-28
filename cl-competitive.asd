(asdf:defsystem :cl-competitive
    :components ((:file "src/lib/queue")
                 (:file "src/lib/stack"))
    :in-order-to ((test-op (test-op :cl-competitive/tests))))

(asdf:defsystem :cl-competitive/tests
    :depends-on (:cl-competitive :fiveam)
    :components ((:file "tests/lib/queue")
                 (:file "tests/lib/stack"))
    :perform (test-op (o c) (symbol-call :fiveam '#:run! :foobar)))
