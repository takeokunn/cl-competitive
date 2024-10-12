(defpackage cl-competitive/tests/lib/data-structures/matrix
  (:use :cl
        :fiveam
        :cl-competitive/tests/utils
        :cl-competitive/lib/data-structures/matrix))
(in-package :cl-competitive/tests/lib/data-structures/matrix)

(def-suite lib-data-structures-matrix)
(in-suite lib-data-structures-matrix)

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

(test unit-matrix-zero-p
  (let ((m (make-matrix :rows 2 :cols 2 :data '((0 0) (0 0)))))
    (is (matrix-zero-p m))))

(test unit-matrix-negate
  (let ((m (make-matrix :rows 2 :cols 2 :data '((1 2) (3 4)))))
    (is (equalp (matrix-negate m)
                '((-1 -2) (-3 -4))))))

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
    (is (equalp (matrix-get-data (matrix-power mi 10))
                '((1 0)
                  (0 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            property based test           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test property-matrix-add
  ;; 交換法則
  (for-all ((a (gen-matrix))
            (b (gen-matrix)))
    (let ((a-matrix (make-matrix :rows 2 :cols 2 :data a))
          (b-matrix (make-matrix :rows 2 :cols 2 :data b)))
      (is (equalp (matrix-get-data (matrix-add a-matrix b-matrix))
                  (matrix-get-data (matrix-add b-matrix a-matrix))))))
  ;; 結合法則
  (for-all ((a (gen-matrix))
            (b (gen-matrix))
            (c (gen-matrix)))
    (let ((a-matrix (make-matrix :rows 2 :cols 2 :data a))
          (b-matrix (make-matrix :rows 2 :cols 2 :data b))
          (c-matrix (make-matrix :rows 2 :cols 2 :data c)))
      (is (equalp (matrix-get-data (matrix-add (matrix-add a-matrix b-matrix) c-matrix))
                  (matrix-get-data (matrix-add a-matrix (matrix-add b-matrix c-matrix))))))))

(test property-matrix-sub
  ;; A - A = 0
  (for-all ((data (gen-matrix)))
    (let* ((m (make-matrix :rows 2 :cols 2 :data data)))
      (is (matrix-zero-p (matrix-sub m m)))))

  ;; A - B = A + (-B)
  (for-all ((a (gen-matrix))
            (b (gen-matrix)))
    (let* ((a-matrix (make-matrix :rows 2 :cols 2 :data a))
           (b-matrix (make-matrix :rows 2 :cols 2 :data b))
           (b-negate-matrix (make-matrix :rows 2 :cols 2 :data (matrix-negate b-matrix))))
      (is (equalp (matrix-get-data (matrix-sub a-matrix b-matrix))
                  (matrix-get-data (matrix-add a-matrix b-negate-matrix)))))))

(test property-matrix-multiple
  ;; 単位行列 A * I = A
  (for-all ((data (gen-matrix)))
    (let* ((m (make-matrix :rows 2 :cols 2 :data data))
           (i (make-matrix-identity :size 2)))
      (is (equalp (matrix-get-data (matrix-multiple m i))
                  (matrix-get-data m)))))

  ;; 結合法則 A * (B * C) = (A * B) * C
  (for-all ((a (gen-matrix))
            (b (gen-matrix))
            (c (gen-matrix)))
    (let ((a-matrix (make-matrix :rows 2 :cols 2 :data a))
          (b-matrix (make-matrix :rows 2 :cols 2 :data b))
          (c-matrix (make-matrix :rows 2 :cols 2 :data c)))
      (is (equalp (matrix-get-data
                   (matrix-multiple (matrix-multiple a-matrix b-matrix)
                                    c-matrix))
                  (matrix-get-data
                   (matrix-multiple a-matrix
                                    (matrix-multiple b-matrix c-matrix)))))))

  ;; 分配法則 A * (B + C) = A * B + A * C
  (for-all ((a (gen-matrix))
            (b (gen-matrix))
            (c (gen-matrix)))
    (let ((a-matrix (make-matrix :rows 2 :cols 2 :data a))
          (b-matrix (make-matrix :rows 2 :cols 2 :data b))
          (c-matrix (make-matrix :rows 2 :cols 2 :data c)))
      (is (equalp (matrix-get-data
                   (matrix-multiple a-matrix
                                    (matrix-add b-matrix c-matrix)))
                  (matrix-get-data
                   (matrix-add (matrix-multiple a-matrix b-matrix)
                               (matrix-multiple a-matrix c-matrix))))))))

(test property-matrix-power
  ;; I^n = I
  (for-all ((n (gen-integer :min 1 :max 10))
            (size (gen-integer :min 1 :max 10)))
    (let* ((m-i (make-matrix-identity :size size)))
      (is (equalp (matrix-get-data m-i)
                  (matrix-get-data (matrix-power m-i n))))))

  ;; A^1 = A
  (for-all ((m (gen-matrix)))
    (let* ((m (make-matrix :rows 2 :cols 2 :data m)))
      (is (equalp (matrix-get-data m)
                  (matrix-get-data (matrix-power m 1))))))

  ;; A^x * A^y = A^(x+y)
  (for-all ((m (gen-matrix))
            (x (gen-integer :min 1 :max 10))
            (y (gen-integer :min 1 :max 10)))
    (let* ((m (make-matrix :rows 2 :cols 2 :data m)))
      (is (equalp
           (matrix-get-data (matrix-multiple (matrix-power m x)
                                             (matrix-power m y)))
           (matrix-get-data (matrix-power m (+ x y))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  run test                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run! 'lib-data-structures-matrix)
