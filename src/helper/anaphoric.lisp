(defpackage cl-competitive/helper/anaphoric
  (:use :cl)
  (:export #:aif
           #:awhen
           #:alambda
           #:acase
           #:acond))
(in-package :cl-competitive/helper/anaphoric)

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(let ((it ,test-form))
     (when it
       (progn ,@body))))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro acase (test-form &body body)
  `(let ((it ,test-form))
     (case it ,@body)))
