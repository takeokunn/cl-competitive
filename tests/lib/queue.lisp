(defpackage cl-competitive/tests/lib/queue
  (:use :cl :fiveam :cl-competitive/lib/queue))
(in-package :cl-competitive/tests/lib/queue)

(def-suite lib-queue)
(in-suite lib-queue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         integration test          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test queue-integration
  ;; make queue
  (let ((q (make-queue)))

    ;; enqueue
    (enqueue q 10)
    (enqueue q 20)
    (enqueue q 30)

    ;; check
    (is (not (empty-p q)))
    (is (string= (debug-print q) "Queue: (10 20 30)"))

    ;; dequeue
    (is (= (dequeue q) 10))
    (is (= (dequeue q) 20))
    (is (= (dequeue q) 30))

    ;; check
    (is (empty-p q))
    (is (string= (debug-print q) "Queue: NIL"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;            unit test             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test queue-get-element
  (let ((q (make-queue)))
    (is (equal (get-elements q) nil)))

  (let ((q (make-queue :elements '(10 20))))
    (is (equal (get-elements q) '(10 20)))))

(test queue-unit-enqueue
  (let ((q (make-queue)))
    ;; run
    (enqueue q 10)
    (enqueue q 20)

    ;; check
    (is (equal (get-elements q) '(10 20)))))

(test queue-unit-dequeue
  (let* ((q (make-queue :elements '(10 20))))
    ;; bofore
    (is (equal (get-elements q) '(10 20)))

    ;; after
    (is (equal (dequeue q) 10))
    (is (equal (get-elements q) '(20)))))

(test queue-empty-p
  (let ((q (make-queue)))
    (is (empty-p q)))

  (let ((q (make-queue :elements '(10 20))))
    (is (not (empty-p q)))))

(test queue-debug-print
  (let ((q (make-queue)))
    (is (string= (debug-print q) "Queue: NIL")))

  (let ((q (make-queue :elements '(10 20))))
    (is (string= (debug-print q) "Queue: (10 20)"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;            property based test             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *num-trials* 1000)

;;; FIFO特性（順序性）
;; enqueueしたリストが、dequeueした結果と一致するかどうか。
;; 例: enqueue(a), enqueue(b), dequeue() → a, dequeue() → b

(test queue-fifo-property
      (for-all ((elements (gen-list :length (gen-integer :min 1 :max 10))))
               (let ((q (make-queue)))
                 ;; すべての要素をenqueue
                 (dolist (e elements)
                   (enqueue q e))

                 ;; dequeueした結果が元のリストと一致することを確認
                 (is (equal elements
                            (loop :for _ :from 1 :to (length elements)
                                  collect (dequeue q)))))))

;;; queueの長さの不変性
;;; 空のqueueにenqueueした後、queueのサイズが1増えていること。
;;; dequeueした後はqueueのサイズが1減っていること。

(test queue-length-invariant
      (for-all ((elements (gen-list :length (gen-integer :min 1 :max 10))))
               (let ((q (make-queue)))
                 ;; すべての要素をenqueue
                 (dolist (e elements)
                   (enqueue q e))

                 ;; enqueue後の長さを確認
                 (is (= (length elements)
                        (length (get-elements q))))

                 ;; すべてdequeueした後、queueが空であることを確認
                 (loop :for _ :from 1 :to (length elements)
                       :do (dequeue q))
                 (is (empty-p q)))))

;;; 空queue操作の安全性
;;; 空のqueueに対してdequeue()した場合に、エラーが発生せずにnilまたは指定された値が返されるか。

(test queue-empty-queue-dequeue
      (for-all ()
               (let ((q (make-queue)))
                 (is (null (dequeue q))))))

;;; run test

(run! 'lib-queue)
