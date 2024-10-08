(defpackage cl-competitive/lib/data-structures/matrix
  (:use :cl)
  (:export #:make-matrix
           #:make-matrix-identity
           #:matrix-get-data
           #:matrix-get-rows
           #:matrix-get-cols
           #:matrix-zero-p
           #:matrix-negate
           #:matrix
           #:matrix-add
           #:matrix-sub
           #:matrix-multiple
           #:matrix-power))
(in-package :cl-competitive/lib/data-structures/matrix)

(defclass matrix ()
  ((rows :accessor matrix-rows :initarg :rows)
   (cols :accessor matrix-cols :initarg :cols)
   (data :accessor matrix-data :initarg :data)))

(defun make-matrix (&key rows cols data)
  (cond ((or (not (integerp rows))
             (<= rows 0))
         (error "Rows must be a positive integer, but got ~A." rows))
        ((or (not (integerp cols))
             (<= cols 0))
         (error "Columns must be a positive integer, but got ~A." cols))
        ((not (= (length data) rows))
         (error "The number of rows (~A) does not match the data length (~A)." rows (length data)))
        ((some #'(lambda (row)
                   (not (= (length row) cols)))
               data)
         (error "The number of columns (~A) does not match the data columns." cols))
        (t
         (make-instance 'matrix :rows rows :cols cols :data data))))

(defun make-matrix-identity (&key size)
  (if (< 0 size)
      (make-matrix :rows size
                   :cols size
                   :data
                   (loop :for i :from 0 :below size
                         :collect
                         (loop :for j :from 0 :below size
                               :collect (if (= i j) 1 0))))
      (error "Size must be a positive integer greater than zero, but got ~A." size)))

(defmethod matrix-get-data ((m matrix))
  (matrix-data m))

(defmethod matrix-get-rows ((m matrix))
  (matrix-rows m))

(defmethod matrix-get-cols ((m matrix))
  (matrix-cols m))

(defmethod matrix-zero-p ((m matrix))
  (every #'(lambda (row)
             (every #'zerop row))
         (matrix-data m)))

(defmethod matrix-negate ((m matrix))
  (mapcar #'(lambda (row)
              (mapcar #'(lambda (x) (- x)) row))
          (matrix-data m)))

(defmethod matrix-add ((m1 matrix) (m2 matrix))
  (let ((r1 (matrix-rows m1))
        (c1 (matrix-cols m1))
        (r2 (matrix-rows m2))
        (c2 (matrix-cols m2)))
    (if (and (= r1 r2)
             (= c1 c2))
        (let ((result (make-instance 'matrix :rows r1 :cols c1)))
          (setf (matrix-data result)
                (mapcar #'(lambda (row1 row2)
                            (mapcar #'+ row1 row2))
                        (matrix-data m1)
                        (matrix-data m2)))
          result)
        (error "Matrix dimensions must match for addition."))))

(defmethod matrix-sub ((m1 matrix) (m2 matrix))
  (let ((r1 (matrix-rows m1))
        (c1 (matrix-cols m1))
        (r2 (matrix-rows m2))
        (c2 (matrix-cols m2)))
    (if (and (= r1 r2)
             (= c1 c2))
        (let ((result (make-instance 'matrix :rows r1 :cols c1)))
          (setf (matrix-data result)
                (mapcar #'(lambda (row1 row2)
                            (mapcar #'- row1 row2))
                        (matrix-data m1)
                        (matrix-data m2)))
          result)
        (error "Matrix dimensions must match for subtraction."))))

(defmethod matrix-multiple ((m1 matrix) (m2 matrix))
  (let ((r1 (matrix-rows m1))
        (c1 (matrix-cols m1))
        (r2 (matrix-rows m2))
        (c2 (matrix-cols m2)))
    (if (= c1 r2)
        (let ((result (make-instance 'matrix :rows r1 :cols c2)))
          (setf (matrix-data result)
                (loop :for i :from 0 :below r1
                      :collect
                      (loop :for j :from 0 :below c2
                            :collect (loop :for k :from 0 :below c1
                                           :sum (* (nth k (nth i (matrix-data m1)))
                                                   (nth j (nth k (matrix-data m2))))))))
          result)
        (error "Matrix dimensions must match for multiplication."))))

(defmethod matrix-power ((m matrix) (n integer))
  (cond
    ((not (= (matrix-cols m)
             (matrix-rows m)))
     (error "Matrix must be square for exponentiation. Given matrix has dimensions ~A x ~A."
            (matrix-rows m)
            (matrix-cols m)))
    ((= n 0) (make-matrix-identity :size (matrix-rows m)))
    ((= n 1) m)
    ((> n 1)
     (loop :with result = m
           :repeat (1- n)
           :do (setf result (matrix-multiple result m))
           :finally (return result)))
    (t
     (error "Matrix power with negative exponent is not supported."))))
