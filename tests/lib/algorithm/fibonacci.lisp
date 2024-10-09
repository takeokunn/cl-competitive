(defpackage cl-competitive/tests/lib/algorithm/fibonacci
  (:use :cl
        :fiveam
        :cl-competitive/lib/algorithm/fibonacci))
(in-package :cl-competitive/tests/lib/algorithm/fibonacci)

(def-suite lib-fibonacci)
(in-suite lib-fibonacci)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                unit test                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test fib
  (is (= 1 (fib 1)))
  (is (= 1 (fib 2)))
  (is (= 5 (fib 5)))
  (is (= 55 (fib 10))))

(test fib-memo
  (is (= 1 (fib-memo 1)))
  (is (= 1 (fib-memo 2)))
  (is (= 5 (fib-memo 5)))
  (is (= 55 (fib-memo 10))))

(test fib-loop
  (is (= 1 (fib-loop 1)))
  (is (= 1 (fib-loop 2)))
  (is (= 5 (fib-loop 5)))
  (is (= 55 (fib-loop 10))))

(test fib-matrix
  (is (= 1 (fib-matrix 1)))
  (is (= 1 (fib-matrix 2)))
  (is (= 5 (fib-matrix 5)))
  (is (= 55 (fib-matrix 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            property based test           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test property-fib
  ;; fib-memo
  (for-all ((n (gen-integer :min 3 :max 30)))
    (is (equalp (fib-memo n)
                (+ (fib-memo (- n 1))
                   (fib-memo (- n 2))))))

  ;; fib-loop
  (for-all ((n (gen-integer :min 3 :max 30)))
    (is (equalp (fib-loop n)
                (+ (fib-loop (- n 1))
                   (fib-loop (- n 2))))))

  ;; fib-matrix
  (for-all ((n (gen-integer :min 3 :max 30)))
    (is (equalp (fib-matrix n)
                (+ (fib-matrix (- n 1))
                   (fib-matrix (- n 2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  run test                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run! 'lib-fibonacci)
