(defpackage cl-competitive/tests/lib/algorithm/fibonacci
  (:use :cl
        :fiveam
        :cl-competitive/tests/utils
        :cl-competitive/lib/algorithm/fibonacci))
(in-package :cl-competitive/tests/lib/algorithm/fibonacci)

(def-suite lib-algorithm-fibonacci)
(in-suite lib-algorithm-fibonacci)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                unit test                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *test-data* '((1 1)
                            (2 1)
                            (3 2)
                            (4 3)
                            (5 5)
                            (6 8)
                            (7 13)
                            (8 21)
                            (9 34)
                            (10 55)))

(test fib
  (with-data-provider ((arg expected) *test-data*)
    (is (= expected (fib arg)))))

(test fib-tail-rec
  (with-data-provider ((arg expected) *test-data*)
    (is (= expected (fib-tail-rec arg)))))

(test fib-memo
  (with-data-provider ((arg expected) *test-data*)
    (is (= expected (fib-memo arg)))))

(test fib-memo
  (with-data-provider ((arg expected) *test-data*)
    (is (= expected (fib-memo arg)))))

(test fib-loop
  (with-data-provider ((arg expected) *test-data*)
    (is (= expected (fib-loop arg)))))

(test fib-matrix
  (with-data-provider ((arg expected) *test-data*)
    (is (= expected (fib-matrix arg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            property based test           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test property-fib
  ;; for fib-memo
  (for-all ((n (gen-integer :min 3 :max 30)))
    (is (equalp (fib-memo n)
                (+ (fib-memo (- n 1))
                   (fib-memo (- n 2))))))

  ;; for fib-tail-rec
  (for-all ((n (gen-integer :min 3 :max 30)))
    (is (equalp (fib-tail-rec n)
                (+ (fib-tail-rec (- n 1))
                   (fib-tail-rec (- n 2))))))

  ;; for fib-loop
  (for-all ((n (gen-integer :min 3 :max 30)))
    (is (equalp (fib-loop n)
                (+ (fib-loop (- n 1))
                   (fib-loop (- n 2))))))

  ;; for fib-matrix
  (for-all ((n (gen-integer :min 3 :max 30)))
    (is (equalp (fib-matrix n)
                (+ (fib-matrix (- n 1))
                   (fib-matrix (- n 2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  run test                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run! 'lib-algorithm-fibonacci)
