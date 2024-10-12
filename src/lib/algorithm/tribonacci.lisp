(defpackage cl-competitive/lib/algorithm/tribonacci
  (:use :cl
   :cl-competitive/helper/memoize
        :cl-competitive/lib/data-structures/matrix)
  (:export #:trib
           #:trib-memo
           #:trib-loop
           #:trib-matrix
           ))
(in-package :cl-competitive/lib/algorithm/tribonacci)

(defun trib (n)
  (cond ((< n 3) 0)
        ((= n 3) 1)
        (t (+ (trib (- n 1))
              (trib (- n 2))
              (trib (- n 3))))))

(defun-memo trib-memo (n)
  (cond ((< n 3) 0)
        ((= n 3) 1)
        (t (+ (trib (- n 1))
              (trib (- n 2))
              (trib (- n 3))))))

(defun trib-loop (n)
  (cond ((< n 3) 0)
        ((= n 3) 1)
        (t (loop :repeat (- n 3)
                 :for a = 0 :then b
                 :for b = 0 :then c
                 :for c = 1 :then d
                 :for d = 1 :then (+ a b c)
                 :finally (return d)))))

(defun trib-matrix (n)
  (cond ((< n 3) 0)
        ((= n 3) 1)
        (t (let* ((m (make-matrix :rows 3
                                  :cols 3
                                  :data '((1 1 1)
                                          (1 0 0)
                                          (0 1 0))))
                  (result (matrix-get-data (matrix-power m (- n 3)))))
             (+ (first (first result)))))))
