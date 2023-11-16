(defpackage :day-20
  (:use :cl)
  (:import-from :utils :read-day-file :print-assert)
  (:export #:exec))
(in-package :day-20)

(defstruct (node (:conc-name nil))
  (val 0 :type integer)
  (original 0 :type integer)
  (previous nil)
  (next nil))

(defun val-p (v)
  (lambda (n) (= v (val n))))

(defun original-p (o)
  (lambda (n) (= o (original n))))

(defun load-all ()
  (let ((nodes (loop for line in (read-day-file "20")
		     for i from 0
		     collecting (make-node :val (parse-integer line) :original i :previous nil :next nil))))
    
    (loop for n1 in nodes
	  for n2 in (rest nodes)
	  do (setf (next n1) n2
		   (previous n2) n1))
    
    (setf (previous (first nodes)) (car (last nodes))
	  (next (car (last nodes))) (first nodes))
    nodes))

(defun splice-out (n)
  (setf (next (previous n)) (next n)
	(previous (next n)) (previous n)))

(defun move-positive (n by)
  (splice-out n)
  (loop repeat by
	do (setf (next n) (next (next n))))

  (let* ((new-next (next n))
	 (new-prev (previous new-next)))
    (setf (previous new-next) n
	  (previous n) new-prev
	  (next new-prev) n)))

(defun move-negative (n by)
  (splice-out n)
  (loop repeat by
	do (setf (previous n) (previous (previous n))))
  
  (let* ((new-prev (previous n))
	 (new-next (next new-prev)))
    (setf (next new-prev) n
	  (next n) new-next
	  (previous new-next) n)))

(defun move-node (n len)
  (let* ((the-val (val n))
	 (moves (rem the-val (1- len))))
    (if (plusp moves) (move-positive n moves))
    (if (minusp moves) (move-negative n (abs moves)))))

(defun n-from-zero (nodes count)
  (loop for n = (find-if (val-p 0) nodes) then (next n)
	repeat count
	finally (return n)))

(defun grove-coords-sum (nodes)
  (reduce #'+ (mapcar #'(lambda (num) (val (n-from-zero nodes num))) '(1000 2000 3000))))

(defun exec-part (multiply-by times)
  (let* ((nodes (loop with nodes = (load-all)
		      for n in nodes do (setf (val n) (* multiply-by (val n)))
		      finally (return nodes)))
	 (len (length nodes)))
    (loop repeat times
	  do (loop for i from 0 below len
		   do (move-node (find-if (original-p i) nodes) len))
	  finally (return (grove-coords-sum nodes)))))

(defun exec ()
  (print-assert "Part 1:" (exec-part 1 1) 16533)
  (print-assert "Part 2:" (exec-part 811589153 10) 4789999181006))
