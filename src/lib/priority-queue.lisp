(defpackage cl-competitive/lib/priority-queue
  (:use :cl)
  (:export #:make-priority-queue
           #:priority-queue-get-heap
           #:priority-queue-enqueue
           #:priority-queue-empty-p
           #:priority-queue-debug-print))
(in-package :cl-competitive/lib/priority-queue)

(defun parent-index (index)
  (floor (/ (1- index) 2)))

(defun left-index (index)
  (+ 1 (* 2 index)))

(defun right-index (index)
  (+ 2 (* 2 index)))

(defun swap (lst i j)
  (let* ((temp-i (nth i lst))
         (temp-j (nth j lst)))
    (setf (nth i lst) temp-j)
    (setf (nth j lst) temp-i))
  lst)

(defclass priority-queue ()
  ((heap
    :initform '()
    :initarg :heap
    :accessor heap)))

(defun make-priority-queue (&key (heap '()))
  (make-instance 'priority-queue :heap heap))

(defmethod priority-queue-get-heap ((pq priority-queue))
  (heap pq))

(defmethod heapify-up ((pq priority-queue) index)
  (let ((parent (parent-index index)))
    (when (and (>= index 1)
               (> (car (nth parent (heap pq)))
                  (car (nth index (heap pq)))))
      (setf (heap pq) (swap (heap pq) index parent))
      (heapify-up pq parent))))

(defun priority-queue-enqueue (pq priority value)
  (setf (heap pq)
        (append (heap pq)
                (list (cons priority value))))
  (heapify-up pq (1- (length (heap pq)))))

(defmethod priority-queue-empty-p ((pq priority-queue))
  (null (heap pq)))

(defmethod priority-queue-debug-print ((pq priority-queue))
  (format nil "Priority Queue: ~a" (heap pq)))
