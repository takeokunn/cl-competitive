(defpackage cl-competitive/tests/lib/priority-queue
  (:use :cl :fiveam :cl-competitive/lib/priority-queue))
(in-package :cl-competitive/tests/lib/priority-queue)

(def-suite lib-priority-queue)
(in-suite lib-priority-queue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                utility                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun left-index (index)
  (+ 1 (* 2 index)))

(defun right-index (index)
  (+ 2 (* 2 index)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  run test                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run! 'lib-priority-queue)
