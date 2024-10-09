(asdf:defsystem :cl-competitive
    :components ((:file "src/helper/memoize")
                 (:file "src/lib/algorithm/fibonacci")
                 (:file "src/lib/data-structures/matrix")
                 (:file "src/lib/data-structures/queue")
                 (:file "src/lib/data-structures/stack")
                 (:file "src/lib/data-structures/priority-queue"))
    :in-order-to ((test-op (test-op :cl-competitive/tests))))

(asdf:defsystem :cl-competitive/tests
    :depends-on (:cl-competitive :fiveam)
    :components ((:file "tests/generator")
                 (:file "tests/helper/memoize")
                 (:file "tests/lib/algorithm/fibonacci")
                 (:file "tests/lib/data-structures/matrix")
                 (:file "tests/lib/data-structures/queue")
                 (:file "tests/lib/data-structures/stack")
                 (:file "tests/lib/data-structures/priority-queue"))
    :perform (test-op (o c) (symbol-call :fiveam '#:run! :foobar)))
