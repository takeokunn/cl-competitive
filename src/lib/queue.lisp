(defpackage cl-competitive/lib/queue
  (:use :cl)
  (:export #:make-queue
           #:get-elements
           #:enqueue
           #:dequeue
           #:empty-p
           #:debug-print))
(in-package :cl-competitive/lib/queue)

(defclass queue ()
  ((elements
    :initform nil
    :initarg :elements
    :accessor elements)))

(defun make-queue (&key (elements nil))
  (make-instance 'queue :elements elements))

(defmethod get-elements ((q queue))
  (elements q))

(defmethod enqueue ((q queue) element)
  (setf (elements q)
        (append (elements q) (list element))))

(defmethod dequeue ((q queue))
  (let ((first-element (first (elements q))))
    (setf (elements q) (rest (elements q)))
    first-element))

(defmethod empty-p ((q queue))
  (null (elements q)))

(defmethod debug-print ((q queue))
  (format nil "Queue: ~a" (elements q)))
