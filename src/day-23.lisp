(defpackage :day-23
  (:use :cl)
  (:import-from :utils :read-day-file :print-assert)
  (:import-from :alexandria :copy-array :rotate)
  (:export #:exec))
(in-package :day-23)

(defun coord (row col) (cons row col))
(defun row (c) (car c))
(defun col (c) (cdr c))
(defun coord+ (c1 c2) (cons (+ (car c1) (car c2)) (+ (cdr c1) (cdr c2))))

(defun does-something-p (grid c)
  (loop for to-add in utils:*to-add/8*
	do (if (gethash (coord+ c to-add) grid)
	       (return t))
	finally (return nil)))

(defun parse-grid (day)
  (loop with ret = (make-hash-table :test 'equal)
	for line in (read-day-file day)
	for row from 0
	do (loop for c across line
		 for col from 0
		 do (if (char= #\# c)
			(setf (gethash (coord row col) ret) t)))
	finally (return ret)))

(defparameter *directions* (vector :north :south :west :east))

(defparameter *direction-checks*
  (vector '(:north (-1 . -1) (-1 . 0) (-1 . 1))
	  '(:south (1 . -1) (1 . 0) (1 . 1))
	  '(:west (-1 . -1) (0 . -1) (1 . -1))
	  '(:east (-1 . 1) (0 . 1) (1 . 1))))

(defun to-propose (coor direction)
  (ecase direction
    (:north (coord+ '(-1 . 0) coor))
    (:south (coord+ '(1 . 0) coor))
    (:west (coord+ '(0 . -1) coor))
    (:east (coord+ '(0 . 1) coor))))

(defun round-checks (r)
  (let ((copy (copy-array *direction-checks*))
	(n (- (mod r (length *direction-checks*)))))
    (rotate copy n)))

(defun find-proposal (grid checks coor)
  (loop for check across checks
	do (destructuring-bind (dir c1 c2 c3) check
	     (if (and (not (gethash (coord+ coor c1) grid))
		      (not (gethash (coord+ coor c2) grid))
		      (not (gethash (coord+ coor c3) grid)))
		 (return (to-propose coor dir))))
	finally (return nil)))

(defun grid-bounds (grid)
  (loop for coor being the hash-keys in grid
	minimizing (row coor) into min-row
	minimizing (col coor) into min-col
	maximizing (row coor) into max-row
	maximizing (col coor) into max-col
	finally (return (values min-row max-row min-col max-col))))

(defun empty-spaces (grid)
  (multiple-value-bind (min-row max-row min-col max-col) (grid-bounds grid)
    (loop for row from min-row to max-row
	  summing (loop for col from min-col to max-col
			counting (not (gethash (coord row col) grid))))))

(defun run-round (grid round)
  (loop with checks = (round-checks round)
	with proposals = (make-hash-table :test 'equal)
	with proposal-counts = (make-hash-table :test 'equal)
	for coor being the hash-keys in grid
	do (when (does-something-p grid coor)
	     (let ((proposed (find-proposal grid checks coor)))
	       (when proposed
		 (setf (gethash coor proposals) proposed)
		 (incf (gethash proposed proposal-counts 0)))))
	finally (return (loop with next-grid = (make-hash-table :test 'equal)
			      with changes = 0
			      for coor being the hash-keys in grid
			      do (let ((proposed (gethash coor proposals)))
				   (if (and proposed (= 1 (gethash proposed proposal-counts)))
				       (setf (gethash proposed next-grid) t
					     changes (1+ changes))
				       (setf (gethash coor next-grid) t)))
			      finally (return (values next-grid changes))))))

(defun part-1 ()
  (loop with current-grid = (parse-grid "23")
	for round from 0 below 10
	do (setf current-grid (run-round current-grid round))
	finally (return (empty-spaces current-grid))))

(defun part-2 ()
  (loop with grid = (parse-grid "23")
	for round from 0
	do (multiple-value-bind (tmp-grid changes) (run-round grid round)
	     (if (zerop changes)
		 (return (1+ round))
		 (setf grid tmp-grid)))))

(defun exec ()
  (print-assert "Part 1:" (part-1) 3990)
  (print-assert "Part 2:" (part-2) 1057))
