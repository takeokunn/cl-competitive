(defpackage cl-competitive/lib/data-structures/priority-queue
  (:use :cl)
  (:export #:parent-index
           #:left-index
           #:right-index
           #:make-priority-queue
           #:priority-queue-get-heap
           #:priority-queue-enqueue
           #:priority-queue-dequeue
           #:priority-queue-empty-p
           #:priority-queue-debug-print))
(in-package :cl-competitive/lib/data-structures/priority-queue)

(defun parent-index (index)
  (floor (/ (1- index) 2)))

(defun left-index (index)
  (+ (* 2 index) 1))

(defun right-index (index)
  (+ (* 2 index) 2))

(defun swap (lst i j)
  (let ((temp (nth i lst)))
    (setf (nth i lst) (nth j lst))
    (setf (nth j lst) temp))
  lst)

(defclass priority-queue ()
  ((heap
    :initform nil
    :initarg :heap
    :accessor heap)))

(defun make-priority-queue (&key (heap nil))
  (make-instance 'priority-queue :heap heap))

(defmethod priority-queue-get-heap ((pq priority-queue))
  (heap pq))

(defmethod heapify-up ((pq priority-queue) index)
  (let ((now-heap (heap pq))
        (parent (parent-index index)))
    (when (and (< 0 index)
               (<= (first (nth index now-heap))
                   (first (nth parent now-heap))))
      (setf (heap pq)
            (swap now-heap index parent))
      (heapify-up pq parent))))

(defmethod priority-queue-enqueue ((pq priority-queue) priority value)
  (let ((now-heap (heap pq)))
    (setf (heap pq)
          (append now-heap
                  (list (cons priority value))))
    (heapify-up pq (length now-heap))))

(defmethod heapify-down ((pq priority-queue) index)
  (let* ((now-heap (heap pq))
         (left (left-index index))
         (right (right-index index))
         (smallest index)
         (len (length now-heap)))
    (when (and (< left len)
               (< (first (nth left now-heap))
                  (first (nth smallest now-heap))))
      (setf smallest left))

    (when (and (< right len)
               (< (first (nth right now-heap))
                  (first (nth smallest now-heap))))
      (setf smallest right))

    (unless (= smallest index)
      (setf (heap pq)
            (swap now-heap index smallest))
      (heapify-down pq smallest))))

(defmethod priority-queue-dequeue ((pq priority-queue))
  (when (heap pq)
    (let* ((old-heap (heap pq))
           (root (first old-heap))
           (new-heap (rest old-heap)))
      (setf (heap pq)
            (append (last new-heap)
                    (butlast new-heap)))
      (heapify-down pq 0)
      root)))

(defmethod priority-queue-empty-p ((pq priority-queue))
  (null (heap pq)))

(defmethod priority-queue-debug-print ((pq priority-queue))
  (format nil "Priority Queue: ~a" (heap pq)))
