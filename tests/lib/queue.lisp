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

;;; FIFO特性（順序性）
;; enqueueしたリストが、dequeueした結果と一致するかどうか。
;; 例: enqueue(a), enqueue(b), dequeue() → a, dequeue() → b

;;; TBD

;;; run test

(run! 'lib-queue)
