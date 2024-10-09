(asdf:defsystem :cl-competitive
    :components (
                 ;; helper
                 (:file "src/helper/memoize")

                 ;; data-structures
                 (:file "src/lib/data-structures/matrix")
                 (:file "src/lib/data-structures/queue")
                 (:file "src/lib/data-structures/stack")
                 (:file "src/lib/data-structures/priority-queue")

                 ;; algorithm
                 (:file "src/lib/algorithm/fibonacci"))
    :in-order-to ((test-op (test-op :cl-competitive/tests))))

(asdf:defsystem :cl-competitive/tests
    :depends-on (:cl-competitive :fiveam)
    :components ((:file "tests/generator")

                 ;; helper
                 (:file "tests/helper/memoize")

                 ;; data-structures
                 (:file "tests/lib/data-structures/matrix")
                 (:file "tests/lib/data-structures/queue")
                 (:file "tests/lib/data-structures/stack")
                 (:file "tests/lib/data-structures/priority-queue")

                 ;; algorithm
                 (:file "tests/lib/algorithm/fibonacci"))
    :perform (test-op (o c) (symbol-call :fiveam '#:run! :foobar)))
