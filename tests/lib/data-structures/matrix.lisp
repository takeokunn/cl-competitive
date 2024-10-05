(defpackage cl-competitive/tests/lib/data-structures/matrix
  (:use :cl :fiveam :cl-competitive/lib/data-structures/matrix))
(in-package :cl-competitive/tests/lib/data-structures/matrix)

(def-suite lib-matrix)
(in-suite lib-matrix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            integration test              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test integration-matrix
  (let ((m1 (make-matrix :rows 2 :cols 2 :data '((10 10) (10 10))))
        (m2 (make-matrix :rows 2 :cols 2 :data '((5 5) (5 5))))
        (mi (make-matrix-identity :size 2)))

    ;; add test
    (is (equalp (matrix-get-data (matrix-add m1 m2)) '((15 15) (15 15))))
    (is (equalp (matrix-get-data (matrix-add m1 mi)) '((11 10) (10 11))))

    ;; minus test
    (is (equalp (matrix-get-data (matrix-sub m1 m2)) '((5 5) (5 5))))
    (is (equalp (matrix-get-data (matrix-sub m1 mi)) '((9 10) (10 9))))

    ;; multiple test
    (is (equalp (matrix-get-data (matrix-multiple m1 mi)) '((10 10) (10 10))))
    (is (equalp (matrix-get-data (matrix-multiple mi m1)) '((10 10) (10 10))))

    ;; power test
    (is (equalp (matrix-get-data (matrix-power mi 100))
                (matrix-get-data mi)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               unit test                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test unit-make-matrix-success
  (let ((m (make-matrix :rows 2 :cols 3 :data '((1 2 3)
                                                (4 5 6)))))
    (is (typep m 'matrix))
    (is (= 2 (matrix-get-rows m)))
    (is (= 3 (matrix-get-cols m)))
    (is (equal (matrix-get-data m)
               '((1 2 3)
                 (4 5 6))))))

(test unit-make-matrix-invalid
  ;; invalid rows
  (signals
      (error "Rows must be a positive integer, but got -2.")
    (make-matrix :rows -2 :cols 3 :data '((1 2 3) (4 5 6))))
  (signals
      (error "Rows must be a positive integer, but got 0.")
    (make-matrix :rows 0 :cols 3 :data '((1 2 3) (4 5 6))))

  ;; invalid cols
  (signals
      (error "Columns must be a positive integer, but got -3.")
    (make-matrix :rows 2 :cols -3 :data '((1 2 3) (4 5 6))))
  (signals
      (error "Columns must be a positive integer, but got 0.")
    (make-matrix :rows 2 :cols 0 :data '((1 2 3) (4 5 6))))

  ;; invalid matrix
  (signals
      (error "The number of columns (~A) does not match the data columns." 1)
    (make-matrix :rows 1 :cols 2 :data '((1) (4))))
  (signals
      (error "The number of rows (~A) does not match the data length (~A)." 2 1)
    (make-matrix :rows 2 :cols 1 :data '((1 2) (4)))))

(test unit-make-matrix-identity-success
  (is (equalp (matrix-get-data (make-matrix-identity :size 1))
             '((1))))
  (is (equalp (matrix-get-data (make-matrix-identity :size 2))
             '((1 0)
               (0 1))))
  (is (equalp (matrix-get-data (make-matrix-identity :size 3))
             '((1 0 0)
               (0 1 0)
               (0 0 1)))))

(test unit-matrix-add-success
  (let ((m1 (make-matrix :rows 2 :cols 2 :data '((1 2) (3 4))))
        (m2 (make-matrix :rows 2 :cols 2 :data '((4 3) (2 1)))))
    (is (equal '((5 5) (5 5))
               (matrix-get-data (matrix-add m1 m2))))))

(test unit-matrix-add-failure
  (let ((m1 (make-matrix :rows 3 :cols 3 :data '((1 2 3) (4 5 6) (7 8 9))))
        (m2 (make-matrix :rows 2 :cols 2 :data '((4 3) (2 1)))))
    (signals
        (error "Matrix dimensions must match for addition.")
      (matrix-add m1 m2))))

(test unit-matrix-sub-success
  (let ((m1 (make-matrix :rows 2 :cols 2 :data '((3 3) (3 3))))
        (m2 (make-matrix :rows 2 :cols 2 :data '((1 1) (1 1)))))
    (is (equal '((2 2) (2 2))
               (matrix-get-data (matrix-sub m1 m2))))))

(test unit-matrix-sub-failure
  (let ((m1 (make-matrix :rows 3 :cols 3 :data '((3 3 3) (3 3 3) (3 3 3))))
        (m2 (make-matrix :rows 2 :cols 2 :data '((1 1) (1 1)))))
    (signals
        (error "Matrix dimensions must match for subtraction.")
      (matrix-sub m1 m2))))

(test unit-matrix-multiple-success
  (let ((m1 (make-matrix :rows 2 :cols 2 :data '((1 1) (1 1))))
        (m2 (make-matrix :rows 2 :cols 2 :data '((1 1) (1 1)))))
    (is (equal '((2 2) (2 2))
               (matrix-get-data (matrix-multiple m1 m2))))))

(test unit-matrix-multiple-failure
  (let ((m1 (make-matrix :rows 3 :cols 3 :data '((1 1 1) (1 1 1) (1 1 1))))
        (m2 (make-matrix :rows 2 :cols 2 :data '((1 1) (1 1)))))
    (signals
        (error "Matrix dimensions must match for multiplication.")
      (matrix-multiple m1 m2))))

(test unit-matrix-power-success
  (let ((mi (make-matrix-identity :size 2)))
    (is (equalp (matrix-data (matrix-power mi 10))
                '((1 0)
                  (0 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            property based test           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test property-matrix-add
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  run test                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run! 'lib-matrix)
