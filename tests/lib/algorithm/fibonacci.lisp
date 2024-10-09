(defpackage cl-competitive/tests/lib/algorithm/fibonacci
  (:use :cl
        :fiveam
        :cl-competitive/lib/algorithm/fibonacci))
(in-package :cl-competitive/tests/lib/algorithm/fibonacci)

(def-suite lib-fibonacci)
(in-suite lib-fibonacci)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            integration test              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test fibonacci
  (is (= 1 (fib))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  run test                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run! 'lib-fibonacci)
