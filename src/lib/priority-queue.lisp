(defpackage cl-competitive/lib/priority-queue
  (:use :cl)
  (:export #:make-priority-queue
           #:priority-queue-get-heap
           #:priority-queue-enqueue
           #:priority-queue-dequeue
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
  (let ((now-heap (heap pq))
        (parent (parent-index index)))
    (when (and (>= index 1)
               (> (first (nth parent now-heap))
                  (first (nth index now-heap))))
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
  (let* ((left (left-index index))
         (right (right-index index))
         (smallest index)
         (now-heap (heap pq))
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
  (when (null (heap pq))
    (return-from priority-queue-dequeue))

  (let* ((now-heap (heap pq))
         (root (first now-heap)))
    (if (null (rest now-heap))
        (setf (heap pq) nil)
        (progn
          (setf (heap pq)
                (rest now-heap))
          (heapify-down pq 0)))
    root))

(defmethod priority-queue-empty-p ((pq priority-queue))
  (null (heap pq)))

(defmethod priority-queue-debug-print ((pq priority-queue))
  (format nil "Priority Queue: ~a" (heap pq)))
