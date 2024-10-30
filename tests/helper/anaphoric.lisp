(defpackage cl-competitive/tests/helper/anaphoric
  (:use :cl
        :fiveam
        :cl-competitive/helper/anaphoric))
(in-package :cl-competitive/tests/helper/anaphoric)

(def-suite helper-anaphoric)
(in-suite helper-anaphoric)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                unit test                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test unit-aif
  ;; test macroexpand
  (let ((test1 (macroexpand-1 '(aif (+ 1 2) it it)))
        (test2 '(let ((cl-competitive/helper/anaphoric::it (+ 1 2)))
                 (if cl-competitive/helper/anaphoric::it
                     cl-competitive/helper/anaphoric::it
                     cl-competitive/helper/anaphoric::it))))
    (is (string= (format nil "~A" test1)
                 (format nil "~A" test2))))

  ;; test function
  (is (eq (+ 2 7)
          (aif (+ 2 7) cl-competitive/helper/anaphoric::it nil)))

  (is (null (aif nil t cl-competitive/helper/anaphoric::it))))

(test unit-awhen
  ;; test macroexpand
  (let ((test1 (macroexpand-1 '(awhen (+ 1 2) it)))
        (test2 '(let ((cl-competitive/helper/anaphoric::it (+ 1 2)))
                 (when cl-competitive/helper/anaphoric::it
                   (progn cl-competitive/helper/anaphoric::it)))))
    (is (string= (format nil "~A" test1)
                 (format nil "~A" test2))))

  ;; test function
  (is (eq (+ 2 7)
          (awhen (+ 2 7) cl-competitive/helper/anaphoric::it)))

  (is (null (aif nil t))))

(test unit-alambda
  ;; test macroexpand
  (let ((test1 (macroexpand-1 '(alambda (n) self)))
        (test2 '(labels ((cl-competitive/helper/anaphoric::self (n)
                          self))
                 #'cl-competitive/helper/anaphoric::self)))
    (is (string= (format nil "~A" test1)
                 (format nil "~A" test2))))

  ;; test function
  (is (eq 120
          (funcall (alambda (n)
                     (if (zerop n)
                         1
                         (* n (cl-competitive/helper/anaphoric::self (1- n)))))
                   5))))

(test unit-acase
  ;; test macroexpand
  (let ((test1 (macroexpand-1 '(acase 100
                                (1 t)
                                (otherwise nil))))
        (test2 '(let ((cl-competitive/helper/anaphoric::it 100))
                 (case cl-competitive/helper/anaphoric::it
                   (1 t)
                   (otherwise nil)))))
    (is (string= (format nil "~A" test1)
                 (format nil "~A" test2))))

  ;; test function
  (is (null (acase 101
            (100 t)
            (otherwise nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  run test                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run! 'helper-anaphoric)
