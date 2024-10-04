(defpackage cl-competitive/lib/data-structures/stack
  (:use :cl)
  (:export #:make-stack
           #:stack-get-elements
           #:stack-push
           #:stack-pop
           #:stack-peek
           #:stack-empty-p
           #:stack-debug-print))
(in-package :cl-competitive/lib/data-structures/stack)

(defclass stack ()
  ((elements
    :initform nil
    :initarg :elements
    :accessor elements)))

(defun make-stack (&key (elements nil))
  (cl:make-instance 'stack :elements elements))

(defmethod stack-get-elements ((s stack))
  (elements s))

(defmethod stack-push ((s stack) element)
  (setf (elements s)
        (append (elements s) (list element))))

(defmethod stack-pop ((s stack))
  (let ((last-element (first (last (elements s)))))
    (setf (elements s) (butlast (elements s)))
    last-element))

(defmethod stack-peek ((s stack))
  (first (last (elements s))))

(defmethod stack-empty-p ((s stack))
  (null (elements s)))

(defmethod stack-debug-print ((s stack))
  (format nil "Stack: ~a" (elements s)))
