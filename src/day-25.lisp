(defpackage :day-25
  (:use :cl)
  (:import-from :utils :read-day-file :print-assert)
  (:export #:exec))
(in-package :day-25)

(defun snafu->10 (str)
  (loop with multiplier = 1
	for c across (reverse str)
	summing (cond ((char= #\0 c) 0)
		      ((char= #\1 c) multiplier)
		      ((char= #\2 c) (* 2 multiplier))
		      ((char= #\- c) (- multiplier))
		      ((char= #\= c) (* -2 multiplier))) into ret
	do (setf multiplier (* multiplier 5))
	finally (return ret)))

(defun sum-all (day)
  (let ((snafu-nums (read-day-file day)))
    (reduce #'+ (mapcar #'snafu->10 snafu-nums))))

(defun to-base-5 (num)
  (loop with res = nil
	with quotient = num
	while (not (zerop quotient))
	do (multiple-value-bind (quot rem) (floor quotient 5)
	     (push rem res)
	     (setf quotient quot))
	finally (return res)))

(defun fix-up (ary at)
  (if (< at (length ary))
      (let ((me (aref ary at)))
	(when (<= me 2)
	  (fix-up ary (1+ at))
	  (return-from fix-up))
	
	(if (= (1+ at) (length ary))
	    (vector-push-extend 0 ary))
	
	(setf (aref ary at) (- me 5))
	(incf (aref ary (1+ at)))
	(fix-up ary (1+ at)))))
	   
(defun to-snafu (lst)
  (let ((ary (make-array 1 :adjustable t :fill-pointer 0)))
    (loop for num in (reverse lst)
	  do (vector-push-extend num ary))
    
    (fix-up ary 0)
    (flet ((transform (n)
	     (cond ((<= 0 n 2)
		    (digit-char n))
		   ((= -1 n) #\-)
		   ((= -2 n) #\=))))
      (map 'string #'transform (reverse ary)))))

(defun exec ()
  (print-assert "Part 1:" (to-snafu (to-base-5 (sum-all "25"))) "2--2-0=--0--100-=210"))

    

	
    
