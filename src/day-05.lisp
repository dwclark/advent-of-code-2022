(defpackage :day-05
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :split-sequence :split-sequence)
  (:import-from :cl-ppcre :do-register-groups)
  (:export #:part-1 #:part-2))

(in-package :day-05)

(defun allocate-stacks (str-stack)
  (let ((stacks (make-array 0 :adjustable t :fill-pointer 0))
        (stack-num-indices (make-array 0 :adjustable t :fill-pointer 0)))
    (loop with stack-num-line = (first str-stack)
          for idx from 0 below (length stack-num-line)
          do (if (digit-char-p (aref stack-num-line idx))
                 (progn
                   (vector-push-extend nil stacks)
                   (vector-push-extend idx stack-num-indices))))
    (loop for str in (rest str-stack)
          do (loop for stack-num-index across stack-num-indices
                   for i from 0 below (length stack-num-indices)
                   do (if (alpha-char-p (aref str stack-num-index))
                          (push (aref str stack-num-index) (aref stacks i)))))
    stacks))

(defun allocate-move (line)
  (do-register-groups ((#'parse-integer pos src dest)) ("^move (\\d+) from (\\d+) to (\\d+)$" line)
    (return (list pos src dest))))

(defun play-game (stack-func)
  (let* ((lists (split-sequence "" (read-day-file "05") :test #'string=))
         (stacks (allocate-stacks (reverse (first lists))))
         (moves (mapcar #'allocate-move (second lists))))
    (loop for move in moves
          do (funcall stack-func stacks (first move) (second move) (third move)))
    (map 'string #'car stacks)))

(defun part-1 ()
  (flet ((move-pieces (stacks num from to)
           (dotimes (val num)
             (push (pop (aref stacks (1- from))) (aref stacks (1- to))))))
    (play-game #'move-pieces)))

(defun part-2 ()
  (flet ((move-pieces (stacks num from to)
           (let ((intermediate nil))
             (dotimes (val num)
               (push (pop (aref stacks (1- from))) intermediate))
             (dotimes (val num)
               (push (pop intermediate) (aref stacks (1- to)))))))
    (play-game #'move-pieces)))
