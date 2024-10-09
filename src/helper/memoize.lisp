(defpackage cl-competitive/helper/memoize
  (:use :cl)
  (:export #:defun-memo))
(in-package :cl-competitive/helper/memoize)

(defmacro defun-memo (name lambda-list &body body)
  (let ((cache (gensym "CACHE")))
    `(let ((,cache (make-hash-table :test 'equal)))
       (defun ,name ,lambda-list
         (let ((args (list ,@lambda-list)))
           (or (gethash args ,cache)
               (setf (gethash args ,cache)
                     (progn ,@body))))))))
