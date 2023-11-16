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

(defun equal-node (n1 n2)
  (and (= (val n1) (val n2))
       (= (original n1) (original n2))))

(defun to-string (n)
  (format nil "(~A ~A)" (val n) (original n)))

(defun print-nodes (s-node)
  (format t "~A " (to-string s-node))
  (loop for n = (next s-node) then (next n)
	while (not (eq n s-node))
	do (format t "~A " (to-string n))
	finally (format t "~%")))

(defun swap-prev (n)
  (rotatef (val (previous n)) (val n))
  (rotatef (original (previous n)) (original n)))

(defun swap-next (n)
  (rotatef (val (next n)) (val n))
  (rotatef (original (next n)) (original n)))

(defun find-node (nodes data &optional (data-func #'original) (by 0))
  (let ((node (find-if #'(lambda (n) (= (funcall data-func n) data)) nodes)))
    (if (not (zerop by))
	(loop with moves = (rem by (length nodes))
	      with func = (if (< moves 0) #'previous #'next)
	      repeat (abs moves)
	      do (setf node (funcall func node))))
    node))

(defun move-node (s-node len &optional (use-rem t) (by (val s-node)))
  (let* ((n-func (if (plusp by) #'next #'previous))
	 (s-func (if (plusp by) #'swap-next #'swap-prev))
	 (abs-val (abs by))
	 (remainder (if use-rem (rem abs-val len) abs-val))
	 (divisions (if use-rem (floor (/ abs-val len)) abs-val))
	 (times (+ remainder divisions)))
    (if (zerop times)
	(return-from move-node))
    
    (loop repeat times
	  for n = s-node then (funcall n-func n)
	  do (funcall s-func n))))

(defun move-all-nodes (nodes)
  (loop with len = (length nodes)
	for pos from 0 below len
	do (move-node (find-node nodes pos) len)))

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

(defun part-1 (nodes)
  (move-all-nodes nodes)
  (reduce #'+ (mapcar #'(lambda (by)
			  (val (find-node nodes 0 by #'val))) (list 1000 2000 3000))))
  
(defun exec ()
  (let ((nodes (load-all)))
    ;(loop for i from 0 below 20
    (let ((copy-1 (load-all))
	  (copy-2 (load-all))
	  (value (val (nth 1 nodes))))
      (move-node (nth 1 copy-1) (length nodes) nil)
      (move-node (nth 1 copy-2) (length nodes) t)
      (format t "~A; ~A: are equal: ~A~%" 1 value (every #'equal-node copy-1 copy-2)))))

