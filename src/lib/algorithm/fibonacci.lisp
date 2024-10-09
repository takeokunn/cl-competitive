(defpackage cl-competitive/lib/algorithm/fibonacci
  (:use :cl
        :cl-competitive/helper/memoize
        :cl-competitive/lib/data-structures/matrix)
  (:export #:fib
           #:fib-memo
           #:fib-loop
           #:fib-matrix))
(in-package :cl-competitive/lib/algorithm/fibonacci)

(defun fib (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(defun-memo fib-memo (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(defun fib-loop (n)
  (loop :with a = 0
        :with b = 1
        :for index :from 0 :below n
        :do (rotatef a b)
        :do (incf b a)
        :finally (return a)))

(defun fib-matrix (n)
  (let* ((m (make-matrix :rows 2 :cols 2 :data '((1 1)
                                                 (1 0))))
         (result (matrix-get-data (matrix-power m (1- n)))))
    (first (first result))))
