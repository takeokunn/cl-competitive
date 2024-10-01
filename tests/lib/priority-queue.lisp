(defpackage cl-competitive/tests/lib/priority-queue
  (:use :cl :fiveam :cl-competitive/lib/priority-queue))
(in-package :cl-competitive/tests/lib/priority-queue)

(def-suite lib-priority-queue)
(in-suite lib-priority-queue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                utility                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun valid-min-heap-p (heap)
  (loop
    :with heap-length = (length heap)
    :for index :from 0 :below (floor heap-length 2)
    :do (let ((parent (nth index heap))
              (left (nth (left-index index) heap))
              (right (nth (right-index index) heap)))

          ;;; If the left child node is smaller than the parent node
          (when (and (< (left-index index) heap-length)
                     (not (<= (car parent) (car left))))
            (return nil))

          ;;; If the right child node is smaller than the parent node
          (when (and (< (right-index index) heap-length)
                     (not (<= (car parent) (car right))))
            (return nil))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            integration test              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test integration-priority-queue
  ;; make priority-queue
  (let ((pq (make-priority-queue)))
    ;; enqueue
    (priority-queue-enqueue pq 3 "hello 3")
    (priority-queue-enqueue pq 1 "hello 1")
    (priority-queue-enqueue pq 4 "hello 4")
    (priority-queue-enqueue pq 5 "hello 5")
    (priority-queue-enqueue pq 2 "hello 2")

    ;; check
    (is (= 5 (length (priority-queue-get-heap pq))))
    (is (valid-min-heap-p (priority-queue-get-heap pq)))

    ;; dequeue
    (is (equal '(1 . "hello 1") (priority-queue-dequeue pq)))
    (is (equal '(2 . "hello 2") (priority-queue-dequeue pq)))
    (is (equal '(3 . "hello 3") (priority-queue-dequeue pq)))
    (is (equal '(4 . "hello 4") (priority-queue-dequeue pq)))
    (is (equal '(5 . "hello 5") (priority-queue-dequeue pq)))

    ;; check
    (is (zerop (length (priority-queue-get-heap pq))))
    (is (priority-queue-empty-p pq))
    (is (null (priority-queue-get-heap pq)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               unit test                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test unit-priority-queue-empty-p
  (let ((pq (make-priority-queue)))
    (is (priority-queue-empty-p pq)))

  (let ((pq (make-priority-queue :heap '(1 . "hello"))))
    (is (not (priority-queue-empty-p pq)))))

(test unit-priority-queue-get-heap
  (let ((pq (make-priority-queue)))
    (is (null (priority-queue-get-heap pq))))

  (let ((pq (make-priority-queue :heap '(1 . "hello"))))
    (is (equal (priority-queue-get-heap pq)
               '(1 . "hello")))))

(test unit-priority-queue-enqueue
  (let ((pq (make-priority-queue)))
    (priority-queue-enqueue pq 3 "hello")
    (priority-queue-enqueue pq 1 "hello")
    (priority-queue-enqueue pq 5 "hello")
    (priority-queue-enqueue pq 7 "hello")
    (priority-queue-enqueue pq 2 "hello")
    (priority-queue-enqueue pq 10 "hello")
    (priority-queue-enqueue pq 8 "hello")
    (priority-queue-enqueue pq 9 "hello")
    (priority-queue-enqueue pq 4 "hello")
    (priority-queue-enqueue pq 6 "hello")

    (is (= 10 (length (priority-queue-get-heap pq))))
    (is (valid-min-heap-p (priority-queue-get-heap pq)))))

(test unit-priority-queue-dequeue
  (let ((pq (make-priority-queue :heap '((1 . "a") (2 . "b") (3 . "c")))))
    (is (valid-min-heap-p (priority-queue-get-heap pq)))
    (is (equal '(1 . "a") (priority-queue-dequeue pq)))
    (is (equal '(2 . "b") (priority-queue-dequeue pq)))
    (is (equal '(3 . "c") (priority-queue-dequeue pq)))
    (is (zerop (length (priority-queue-get-heap pq))))))

(test unit-priority-queue-debug-print
  (let ((pq (make-priority-queue)))
    (is (string= (priority-queue-debug-print pq)
                 "Priority Queue: NIL")))

  (let ((pq (make-priority-queue :heap '((1 . "hello") (2 . "world")))))
    (is (string= (priority-queue-debug-print pq)
                 "Priority Queue: ((1 . hello) (2 . world))"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            property based test           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *num-trials* 1000)

(defun gen-pair ()
  (lambda ()
    (loop
      :repeat (funcall (gen-integer :min 0 :max 100))
      :collect (cons (funcall (gen-integer :min 1 :max 100))
                     (funcall (gen-character))))))

;;; Dequeue後の順序検証

(test property-priority-queue-dequeue-order
  (for-all ((lst (gen-pair)))
    (let ((pq (make-priority-queue)))
      (loop :for (priority . value) :in lst
            :do (priority-queue-enqueue pq priority value))

      ;; min-heapになっているかどうか
      (is (valid-min-heap-p (priority-queue-get-heap pq)))

      ;; 根には常に最小の優先度がある
      (let ((heap (priority-queue-get-heap pq)))
        (loop :for index :from 1 :below (length heap)
              :for parent = (parent-index index)
              :for child = index
              :do (is (<= (first (nth parent heap))
                          (first (nth child heap)))))))))

;; enqueue/dequeue の整合性テスト

(test property-priority-queue-enqueue-dequeue
  (for-all ((lst (gen-pair)))
    (let ((pq (make-priority-queue)))
      (loop :for (priority . value) :in lst
            :do (priority-queue-enqueue pq priority value))

      ;; min-heapになっているかどうか
      (is (valid-min-heap-p (priority-queue-get-heap pq)))

      ;; 根には常に最小の優先度がある
      (let ((heap (priority-queue-get-heap pq)))
        (loop :repeat (floor (1- (length lst)) 2)
              :for (priority . value) = (priority-queue-dequeue pq)
              :for now-heap = (priority-queue-get-heap pq)
              :do (is (or (null now-heap)
                          (every (lambda (el)
                                   (<= priority (first el)))
                                 now-heap))))))))

;;; 空priority-queue操作の安全性
;;; 空のpriority-queueに対してdequeue()した場合に、エラーが発生せずにnilまたは指定された値が返されるか。

(test property-priority-queue-empty-dequeue
  (for-all ()
    (let ((pq (make-priority-queue)))
      (is (null (priority-queue-dequeue pq))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  run test                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run! 'lib-priority-queue)
