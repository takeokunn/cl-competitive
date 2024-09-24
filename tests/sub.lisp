(defpackage cl-competitive/tests/sub
  (:use :cl :cl-competitive/sub :fiveam))
(in-package :cl-competitive/tests/sub)

(def-suite my-suite)

(in-suite my-suite)

(test test-demo-sub
      (is (equal (cl-competitive/sub:sub) "hello world")))

(run! 'my-suite)
