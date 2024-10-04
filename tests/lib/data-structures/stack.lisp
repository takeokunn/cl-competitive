(defpackage cl-competitive/tests/lib/data-structures/stack
  (:use :cl :fiveam :cl-competitive/lib/data-structures/stack))
(in-package :cl-competitive/tests/lib/data-structures/stack)

(def-suite lib-stack)
(in-suite lib-stack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            integration test              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test integration-stack
  ;; make stack
  (let ((s (make-stack)))
    ;; push
    (stack-push s 10)
    (stack-push s 20)
    (stack-push s 30)

    ;; check
    (is (not (stack-empty-p s)))
    (is (string= (stack-debug-print s) "Stack: (10 20 30)"))

    ;; pop
    (is (= (stack-pop s) 30))
    (is (= (stack-pop s) 20))
    (is (= (stack-pop s) 10))

    ;; check
    (is (stack-empty-p s))
    (is (string= (stack-debug-print s) "Stack: NIL"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               unit test                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test unit-stack-get-elements
  (let ((s (make-stack)))
    (is (equal (stack-get-elements s) nil)))

  (let ((s (make-stack :elements '(10 20))))
    (is (equal (stack-get-elements s) '(10 20)))))

(test unit-stack-push
  (let ((s (make-stack)))
    ;; run
    (stack-push s 10)
    (stack-push s 20)

    ;; check
    (is (equal (stack-get-elements s) '(10 20)))))

(test unit-stack-pop
  (let ((s (make-stack :elements '(10 20))))
    ;; before
    (is (equal (stack-get-elements s) '(10 20)))

    ;; run
    (is (equal (stack-pop s) 20))
    (is (equal (stack-pop s) 10))))

(test unit-stack-peek
  (let ((s (make-stack :elements '(10 20))))
    ;; before
    (is (equal (stack-get-elements s) '(10 20)))

    ;; run
    (is (equal (stack-peek s) 20))

    ;; after
    (is (equal (stack-get-elements s) '(10 20)))))

(test unit-stack-empty-p
  (let ((s (make-stack)))
    (is (stack-empty-p s)))

  (let ((s (make-stack :elements '(10 20))))
    (is (not (stack-empty-p s)))))

(test unit-stack-debug-print
  (let ((s (make-stack)))
    (is (string= (stack-debug-print s) "Stack: NIL")))

  (let ((s (make-stack :elements '(10 20))))
    (is (string= (stack-debug-print s) "Stack: (10 20)"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            property based test           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *num-trials* 1000)

;;; LIFO特性（順序性）
;; pushしたリストが、popした結果と一致するかどうか。
;; 例: push(a), push(b), pop() → b, pop() → a

(test property-stack-lifo
  (for-all ((elements (gen-list)))
    (let ((s (make-stack)))
      ;; すべての要素をpush
      (dolist (e elements)
        (stack-push s e))

      ;; dequeueした結果が元のリストと一致することを確認
      (is (equal (reverse elements)
                 (loop :repeat (length elements)
                       :collect (stack-pop s)))))))

;;; stackの長さの不変性
;;; 空のstackにpushした後、stackのサイズが1増えていること。
;;; popした後はstackのサイズが1減っていること。

(test property-stack-length-invariant
  (for-all ((elements (gen-list)))
    (let ((s (make-stack)))
      ;; すべての要素をpush
      (loop :for element :in elements
            :do (stack-push s element)
            :do (is (length (stack-get-elements s)) index)
            :with index = 1
            :do (setf index (1+ index)))

      ;; push後の長さを確認
      (is (= (length elements)
             (length (stack-get-elements s))))

      ;; すべてpopした後、stackが空であることを確認
      (loop :repeat (length elements)
            :do (stack-pop s)
            :with index = (length elements)
            :do (setf index (1- index)))
      (is (stack-empty-p s)))))

;;; 空stack操作の安全性
;;; 空のstackに対してpopした場合に、エラーが発生せずにnilまたは指定された値が返されるか。

(test property-stack-empty-pop
  (for-all ()
    (let ((s (make-stack)))
      (is (null (stack-pop s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  run test                :;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run! 'lib-stack)
