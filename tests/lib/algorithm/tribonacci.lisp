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

(test trib
  (with-data-provider ((arg expected)
                       '((1 0) (2 0) (3 1) (5 2) (10 44)))
    (is (= expected (trib arg)))))

(test trib-memo
  (with-data-provider ((arg expected)
                       '((1 0) (2 0) (3 1) (5 2) (10 44)))
    (is (= expected (trib-memo arg)))))

(test trib-loop
  (with-data-provider ((arg expected)
                       '((2 0) (3 1) (5 2) (10 44)))
    (is (= expected (trib-loop arg)))))

(test trib-matrix
  (with-data-provider ((arg expected)
                       '((1 0) (2 0) (3 1) (5 2) (10 44)))
    (is (= expected (trib-matrix arg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            property based test           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test property-trib
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
