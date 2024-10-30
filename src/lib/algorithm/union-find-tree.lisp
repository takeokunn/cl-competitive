(defpackage cl-competitive/lib/algorithm/union-find-tree
  (:use :cl)
  (:export #:make-union-find))
(in-package :cl-competitive/lib/algorithm/union-find-tree)

(defclass uf-tree ()
  ((parent :accessor uf-parent
           :initarg :parent
           :initform nil)
   (rank :accessor uf-rank
         :initarg :rank
         :initform nil)))

(defun make-uf-tree (n)
  "n 個の要素を持つ Union-Find Tree のインスタンスを作成する。"
  (make-instance 'uf-tree
                 :parent (loop for i from 0 below n collect i)
                 :rank (make-array n :initial-element 0)))

(defmethod uf-find ((uf uf-tree) x)
  "要素 x のルートを見つける。パス圧縮を行いながら検索する。"
  (if (/= (aref (uf-parent uf) x) x)
      (setf (aref (uf-parent uf) x) (uf-find uf (aref (uf-parent uf) x))))
  (aref (uf-parent uf) x))

(defmethod uf-union ((uf uf-tree) x y)
  "要素 x と y が属する集合を統合する。ランク付けを使用して効率化する。"
  (let ((root-x (uf-find uf x))
        (root-y (uf-find uf y)))
    (unless (= root-x root-y)
      (let ((rank-x (aref (uf-rank uf) root-x))
            (rank-y (aref (uf-rank uf) root-y)))
        (cond
          ((< rank-x rank-y)
           (setf (aref (uf-parent uf) root-x) root-y))
          ((> rank-x rank-y)
           (setf (aref (uf-parent uf) root-y) root-x))
          (t
           (setf (aref (uf-parent uf) root-y) root-x)
           (incf (aref (uf-rank uf) root-x))))))))

(defmethod uf-connected-p ((uf uf-tree) x y)
  "要素 x と y が同じ集合に属しているか確認する。"
  (= (uf-find uf x) (uf-find uf y)))

(let ((uf (make-uf-tree 5)))
  (uf-union uf 0 1)
  (uf-union uf 1 2)
  (uf-union uf 3 4)
  (print (uf-connected-p uf 0 2)) ; t
  (print (uf-connected-p uf 0 3)) ; nil
  (uf-union uf 2 4)
  (print (uf-connected-p uf 0 3)) ; t
