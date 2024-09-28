(defpackage cl-competitive/tests/lib/queue
  (:use :cl :fiveam :cl-competitive/lib/queue))
(in-package :cl-competitive/tests/lib/queue)

(def-suite lib-queue)
(in-suite lib-queue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            integration test              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test integration-queue
  ;; make queue
  (let ((q (make-queue)))
    ;; enqueue
    (queue-enqueue q 10)
    (queue-enqueue q 20)
    (queue-enqueue q 30)

    ;; check
    (is (not (queue-empty-p q)))
    (is (string= (queue-debug-print q) "Queue: (10 20 30)"))

    ;; dequeue
    (is (= (queue-dequeue q) 10))
    (is (= (queue-dequeue q) 20))
    (is (= (queue-dequeue q) 30))

    ;; check
    (is (queue-empty-p q))
    (is (string= (queue-debug-print q) "Queue: NIL"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               unit test                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test unit-queue-get-elements
  (let ((q (make-queue)))
    (is (equal (queue-get-elements q) nil)))

  (let ((q (make-queue :elements '(10 20))))
    (is (equal (queue-get-elements q) '(10 20)))))

(test unit-queue-enqueue
  (let ((q (make-queue)))
    ;; run
    (queue-enqueue q 10)
    (queue-enqueue q 20)

    ;; check
    (is (equal (queue-get-elements q) '(10 20)))))

(test unit-queue-dequeue
  (let* ((q (make-queue :elements '(10 20))))
    ;; before
    (is (equal (queue-get-elements q) '(10 20)))

    ;; after
    (is (equal (queue-dequeue q) 10))
    (is (equal (queue-get-elements q) '(20)))))

(test unit-queue-peek
  (let* ((q (make-queue :elements '(10 20))))
    (is (equal (queue-peek q) 10))))

(test unit-queue-empty-p
  (let ((q (make-queue)))
    (is (queue-empty-p q)))

  (let ((q (make-queue :elements '(10 20))))
    (is (not (queue-empty-p q)))))

(test unit-queue-debug-print
  (let ((q (make-queue)))
    (is (string= (queue-debug-print q) "Queue: NIL")))

  (let ((q (make-queue :elements '(10 20))))
    (is (string= (queue-debug-print q) "Queue: (10 20)"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            property based test           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *num-trials* 1000)

;;; FIFO特性（順序性）
;;; enqueueしたリストが、dequeueした結果と一致するかどうか。
;;; 例: enqueue(a), enqueue(b), dequeue() → a, dequeue() → b

(test property-queue-fifo
  (for-all ((elements (gen-list)))
    (let ((q (make-queue)))
      ;; すべての要素をenqueue
      (dolist (e elements)
        (queue-enqueue q e))

      ;; dequeueした結果が元のリストと一致することを確認
      (is (equal elements
                 (loop :repeat (length elements)
                       :collect (queue-dequeue q)))))))

;;; queueの長さの不変性
;;; 空のqueueにenqueueした後、queueのサイズが1増えていること。
;;; dequeueした後はqueueのサイズが1減っていること。

(test property-queue-length-invariant
  (for-all ((elements (gen-list)))
    (let ((q (make-queue)))
      ;; すべての要素をenqueue
      (loop :for element :in elements
            :do (queue-enqueue q element)
            :do (is (length (queue-get-elements q)) index)
            :with index = 1
            :do (setf index (1+ index)))

      ;; enqueue後の長さを確認
      (is (= (length elements)
             (length (queue-get-elements q))))

      ;; すべてdequeueした後、queueが空であることを確認
      (loop :repeat (length elements)
            :do (queue-dequeue q)
            :with index = (length elements)
            :do (setf index (1- index)))
      (is (queue-empty-p q)))))

;;; 空queue操作の安全性
;;; 空のqueueに対してdequeue()した場合に、エラーが発生せずにnilまたは指定された値が返されるか。

(test property-queue-empty-dequeue
  (for-all ()
    (let ((q (make-queue)))
      (is (null (queue-dequeue q))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  run test                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run! 'lib-queue)
