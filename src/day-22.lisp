(defpackage :day-22
  (:use :cl)
  (:import-from :utils :read-day-file :print-assert)
  (:import-from :cl-ppcre :do-register-groups)
  (:export #:exec))
(in-package :day-22)

(defun coord (row col) (cons row col))
(defun row (coord) (car coord))
(defun col (coord) (cdr coord))

(defstruct (mega-grid (:conc-name nil))
  (original-rows 0)
  (original-cols 0)
  (mega-rows 0)
  (mega-cols 0)
  (square-size 0)
  (grid nil))

(defun set-val (mega-grid mega sub val)
  (setf (aref (aref (grid mega-grid) (row mega) (col mega)) (row sub) (col sub)) val))

(defun get-val (mega-grid mega sub)
  (aref (aref (grid mega-grid) (row mega) (col mega)) (row sub) (col sub)))

(defmacro loop-all (m-grid &body body) 
  `(loop for original-row from 0 below (original-rows ,m-grid)
	 do (loop for original-col from 0 below (original-cols ,m-grid)
		  do (multiple-value-bind (mega-row sub-row) (floor original-row (square-size ,m-grid))
		       (multiple-value-bind (mega-col sub-col) (floor original-col (square-size ,m-grid))
			 ,@body)))))

(defun parse-lines (lines)
  (loop for line in lines
	until (zerop (length line))
	collecting line into grid-lines
	finally (return (list grid-lines (car (last lines))))))

(defun parse-directions (line)
  (labels ((to-symbol (dir) (if (= 1 (length dir)) (read-from-string dir) nil)))
    (let ((ret nil))
      (do-register-groups ((#'parse-integer dist) (#'to-symbol dir)) ("([0-9]+)([RL]?)" line)
	(setf ret (append ret (if (not (null dir)) (list dist dir) (list dist)))))
      ret)))

(defun print-mega-grid (m-grid)
  (loop-all m-grid
	    (if (zerop original-col)
		(format t "~%"))
	    (format t "~A" (aref (aref (grid m-grid) mega-row mega-col) sub-row sub-col))))
							     
(defun grid-of-grids (square-size grid-lines)
  (let* ((rows (length grid-lines))
	 (cols (loop for line in grid-lines maximizing (length line)))
	 (m-grid (make-mega-grid :original-rows rows :original-cols cols :square-size square-size
				 :mega-rows (/ rows square-size) :mega-cols (/ cols square-size)
				 :grid (make-array (list (/ rows square-size) (/ cols square-size))))))
    
    (loop for grid-row from 0 below (mega-rows m-grid)
	  do (loop for grid-col from 0 below (mega-cols m-grid)
		   do (setf (aref (grid m-grid) grid-row grid-col)
			    (make-array (list square-size square-size) :initial-element nil))))

    (loop-all m-grid
	      (set-val m-grid (coord mega-row mega-col) (coord sub-row sub-col)
		       (if (< original-col (length (elt grid-lines original-row)))
			   (elt (elt grid-lines original-row) original-col)
			   '#\space)))
    
    (print-mega-grid m-grid)))

(defun load-file ()
  (let* ((lines (read-day-file "22")))
    (destructuring-bind (grid-lines direction-line) (parse-lines lines)
      (parse-directions direction-line)
      (grid-of-grids 4 grid-lines))))
