(defpackage cl-competitive/tests/sandbox
  (:use :cl :fiveam))
(in-package :cl-competitive/tests/sandbox)

(def-suite sandox)
(in-suite sandox)

(test main
      (is (equal (cl-competitive/sandbox:main) "hello world")))

(run! 'sandox)
