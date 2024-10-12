(defpackage cl-competitive/tests/utils
  (:use :cl :fiveam)
  (:export #:with-data-provider

           ;; generator
           #:gen-pair
           #:gen-matrix))
(in-package :cl-competitive/tests/utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 data provider               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-data-provider ((vars data) &body body)
  `(dolist (item ,data)
     (destructuring-bind ,vars item
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   generator                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                :collect (funcall (gen-integer))))))
