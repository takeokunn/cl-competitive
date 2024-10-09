(defpackage cl-competitive/tests/helper/memoize
  (:use :cl
        :fiveam
        :cl-competitive/helper/memoize))
(in-package :cl-competitive/tests/helper/memoize)

(def-suite helper-memoize)
(in-suite helper-memoize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                unit test                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test memoize
  (defun-memo fib (n)
    (if (<= n 1)
        n
        (+ (fib (- n 1))
           (fib (- n 2)))))
  (is (functionp #'fib))

  (for-all ((n (gen-integer :min 3 :max 100)))
    (is (= (fib n)
           (+ (fib (- n 1))
              (fib (- n 2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  run test                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run! 'helper-memoize)
