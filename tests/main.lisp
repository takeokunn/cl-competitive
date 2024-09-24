(defpackage cl-competitive/tests/main
  (:use :cl :cl-competitive/main :fiveam))
(in-package :cl-competitive/tests/main)

(def-suite my-suite)

(in-suite my-suite)

(test test-demo-main
      (is (equal (cl-competitive/main:main) "hello world")))

(run! 'my-suite)
