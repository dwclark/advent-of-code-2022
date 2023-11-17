(defpackage :day-21
  (:use :cl)
  (:import-from :utils :read-day-file :print-assert)
  (:import-from :cl-ppcre :split)
  (:export #:exec))
(in-package :day-21)

(defvar *op-table* nil)

(defun find-ins (s) (gethash s *op-table*))
(defun make-instruction (lst) (cons (first lst) (if (null (second lst)) nil (rest lst))))
(defun name (ins) (car ins))
(defun ops (ins) (cdr ins))

(defun run (ins)
  (if (null ins)
      nil
      (let ((ops (ops ins)))
	(if (= 1 (length ops))
	    (first ops)
	    (let ((r1 (run (find-ins (first ops))))
		  (r2 (run (find-ins (third ops)))))
	      (if (or (null r1) (null r2))
		  nil
		  (funcall (second ops) r1 r2)))))))

(defun transform-solve (expected ins)
  (if (null ins)
      expected
      (destructuring-bind (arg1 op arg2) (ops ins)
	(let ((res1 (run (find-ins arg1)))
	      (res2 (run (find-ins arg2))))
	  (cond ((eq '+ op)
		 (if (null res1)
		     (transform-solve (- expected res2) (find-ins arg1))
		     (transform-solve (- expected res1) (find-ins arg2))))
		
		((eq '- op)
		 (if (null res1)
		     (transform-solve (+ expected res2) (find-ins arg1))
		     (transform-solve (- res1 expected) (find-ins arg2))))

		((eq '* op)
		 (if (null res1)
		     (transform-solve (/ expected res2) (find-ins arg1))
		     (transform-solve (/ expected res1) (find-ins arg2))))

		((eq '/ op)
		 (if (null res1)
		     (transform-solve (* expected res2) (find-ins arg1))
		     (transform-solve (/ res1 expected) (find-ins arg2))))

		(t (error "should not be here!")))))))
	
(defun load-all ()
  (labels ((parse-line (line)
	     (mapcar #'read-from-string (split ":? " line))))
    (let ((instructions (mapcar #'make-instruction (mapcar #'parse-line (read-day-file "21"))))
	  (table (make-hash-table :test 'eq)))
      (loop for ins in instructions do (setf (gethash (name ins) table) ins))
      table)))

(defun exec ()
  (let ((*op-table* (load-all)))
    (print-assert "Part 1:" (run (find-ins 'root)) 75147370123646)
    (setf (gethash 'humn *op-table*) nil)
    ;21718827469549 is what the vlzj branch evaluates to
    (print-assert "Part 2:" (transform-solve 21718827469549 (find-ins 'rnsd)) 3423279932937)))

