(defpackage cl-competitive/tests/lib/algorithm/tribonacci
  (:use :cl
   :fiveam
   :cl-competitive/tests/utils
   :cl-competitive/lib/algorithm/tribonacci))
(in-package :cl-competitive/tests/lib/algorithm/tribonacci)

(def-suite lib-algorithm-tribonacci)
(in-suite lib-algorithm-tribonacci)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                unit test                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *test-data* '((1 0)
                            (2 0)
                            (3 1)
                            (4 1)
                            (5 2)
                            (6 4)
                            (7 7)
                            (8 13)
                            (9 24)
                            (10 44)))

(test trib
  (with-data-provider ((arg expected) *test-data*)
    (is (= expected (trib arg)))))

(test trib-tail-rec
  (with-data-provider ((arg expected) *test-data*)
    (is (= expected (trib-tail-rec arg)))))

(test trib-memo
  (with-data-provider ((arg expected) *test-data*)
    (is (= expected (trib-memo arg)))))

(test trib-loop
  (with-data-provider ((arg expected) *test-data*)
    (is (= expected (trib-loop arg)))))

(test trib-matrix
  (with-data-provider ((arg expected) *test-data*)
    (is (= expected (trib-matrix arg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            property based test           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test property-trib
  ;; for trib-tail-rec
  (for-all ((n (gen-integer :min 4 :max 30)))
    (is (equalp (trib-tail-rec n)
                (+ (trib-tail-rec (- n 1))
                   (trib-tail-rec (- n 2))
                   (trib-tail-rec (- n 3))))))

  ;; for trib-memo
  (for-all ((n (gen-integer :min 4 :max 30)))
    (is (equalp (trib-memo n)
                (+ (trib-memo (- n 1))
                   (trib-memo (- n 2))
                   (trib-memo (- n 3))))))

  ;; for trib-loop
  (for-all ((n (gen-integer :min 4 :max 30)))
    (is (equalp (trib-loop n)
                (+ (trib-loop (- n 1))
                   (trib-loop (- n 2))
                   (trib-loop (- n 3))))))

  ;; for trib-matrix
  (for-all ((n (gen-integer :min 4 :max 30)))
    (is (equalp (trib-matrix n)
                (+ (trib-matrix (- n 1))
                   (trib-matrix (- n 2))
                   (trib-matrix (- n 3)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  run test                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run! 'lib-algorithm-tribonacci)
