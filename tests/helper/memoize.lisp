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

(test unit-memoize
  ;; test macroexpand
  (let* ((*GENSYM-COUNTER* 1)
         (test1 (macroexpand-1 '(defun-memo add (a b)
                                 (+ a b))))
         (test2 '(let ((#:CACHE1 (make-hash-table :test 'equal)))
                  (defun add (a b)
                    (let ((cl-competitive/helper/memoize::args (list a b)))
                      (or (gethash cl-competitive/helper/memoize::args #:CACHE1)
                          (setf (gethash cl-competitive/helper/memoize::args #:CACHE1)
                                (progn (+ a b)))))))))
    (is (string= (format nil "~A" test1)
                 (format nil "~A" test2))))

  ;; test function
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
