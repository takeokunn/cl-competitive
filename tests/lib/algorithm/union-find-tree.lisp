(defpackage cl-competitive/tests/lib/algorithm/union-find-tree
  (:use :cl
   :fiveam
   :cl-competitive/tests/utils
   :cl-competitive/lib/algorithm/union-find-tree))
(in-package :cl-competitive/tests/lib/algorithm/union-find-tree)

(def-suite lib-algorithm-union-find-tree)
(in-suite lib-algorithm-union-find-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               unit test                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test unit-make-union-find
  (is (string= "UNION-FIND"
               (type-of (make-union-find 10)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  run test                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run! 'lib-algorithm-union-find-tree)
