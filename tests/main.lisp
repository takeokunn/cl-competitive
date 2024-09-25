(defpackage cl-competitive/tests/main
  (:use :cl :fiveam))
(in-package :cl-competitive/tests/main)

(def-suite my-suite)

(in-suite my-suite)

(test test-main
      (is (equal (cl-competitive/main:main) "hello world")))

(test test-add
      (let ((*num-trials* 1000))
        (for-all* ((a (gen-integer))
                   (b (gen-integer)))
                  (is (= (cl-competitive/main:add a b)
                         (cl-competitive/main:add b a))))))

(run! 'my-suite)
