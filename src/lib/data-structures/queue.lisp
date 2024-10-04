(defpackage cl-competitive/lib/data-structures/queue
  (:use :cl)
  (:export #:make-queue
           #:queue-get-elements
           #:queue-enqueue
           #:queue-dequeue
           #:queue-peek
           #:queue-empty-p
           #:queue-debug-print))
(in-package :cl-competitive/lib/data-structures/queue)

(defclass queue ()
  ((elements
    :initform nil
    :initarg :elements
    :accessor elements)))

(defun make-queue (&key (elements nil))
  (make-instance 'queue :elements elements))

(defmethod queue-get-elements ((q queue))
  (elements q))

(defmethod queue-enqueue ((q queue) element)
  (setf (elements q)
        (append (elements q) (list element))))

(defmethod queue-dequeue ((q queue))
  (let ((first-element (first (elements q))))
    (setf (elements q) (rest (elements q)))
    first-element))

(defmethod queue-peek ((q queue))
  (car (elements q)))

(defmethod queue-empty-p ((q queue))
  (null (elements q)))

(defmethod queue-debug-print ((q queue))
  (format nil "Queue: ~a" (elements q)))
