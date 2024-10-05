(defpackage cl-competitive/tests/helper/generator
  (:use :cl :fiveam)
  (:export #:gen-pair))
(in-package :cl-competitive/tests/helper/generator)

(defun gen-pair ()
  (lambda ()
    (loop :repeat (funcall (gen-integer :min 1 :max 100))
          :collect
          (cons (funcall (gen-integer :min 1 :max 100))
                (funcall (gen-character))))))

(defun gen-matrix (&key (rows 2) (cols 2))
  (lambda ()
    (loop :for i :from 1 :to rows
          :collect
          (loop :for j :from 1 :to cols
                :collect (gen-integer)))))
