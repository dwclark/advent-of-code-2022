(defpackage :day-22
  (:use :cl)
  (:import-from :utils :read-day-file :print-assert)
  (:import-from :cl-ppcre :do-register-groups)
  (:import-from :alexandria :curry)
  (:export #:exec))
(in-package :day-22)

(defun coord (row col) (cons row col))
(defun row (coord) (car coord))
(defun col (coord) (cdr coord))
(defun coord+ (c1 c2) (coord (+ (row c1) (row c2)) (+ (col c1) (col c2))))
(defun coord= (c1 c2) (equal c1 c2))

(defparameter *compass* (vector :north :east :south :west))

(defun compass->move (c)
  (case c
    (:north (coord -1 0))
    (:east (coord 0 1))
    (:south (coord 1 0))
    (:west (coord 0 -1))))

(defun rotate (sym compass)
  (case sym
    (R (case compass (:north :east) (:east :south) (:south :west) (:west :north)))
    (L (case compass (:north :west) (:west :south) (:south :east) (:east :north)))))

(defun opposite (compass)
  (rotate 'R (rotate 'R compass)))

(defun side (grid-coord compass) (cons grid-coord compass))
(defun side-coord (s) (car s))
(defun side-compass (s) (cdr s))

(defun add-connection (dict s1 s2)
  (setf (gethash s1 dict) s2)
  (setf (gethash s2 dict) s1))

(defstruct (mega-grid (:conc-name nil))
  (original-rows 0)
  (original-cols 0)
  (mega-rows 0)
  (mega-cols 0)
  (square-size 0)
  (grid nil))

(defstruct (person (:conc-name nil))
  (mega-coord nil)
  (sub-coord nil)
  (compass nil))

(defun score (p)
  (let ((the-row (+ 1 (* (square-size *mega-grid*) (row (mega-coord p))) (row (sub-coord p))))
	(the-col (+ 1 (* (square-size *mega-grid*) (col (mega-coord p))) (col (sub-coord p))))
	(facing (ecase (compass p) (:east 0) (:south 1) (:west 2) (:north 3))))
    (format t "score row: ~A, col: ~A, facing: ~A" the-row the-col facing)
    (+ (* 1000 the-row) (* 4 the-col) facing)))

(defvar *mega-grid* nil)
(defvar *connections* nil)

(defun within-sub-bounds (sub-coord)
  (and (<= 0 (row sub-coord) (1- (square-size *mega-grid*)))
       (<= 0 (col sub-coord) (1- (square-size *mega-grid*)))))

(defun next-sub (square-size prev-edge next-edge sub-coord)
  (let ((the-row (row sub-coord))
	(the-col (col sub-coord))
	(the-max (1- square-size)))
    (ecase prev-edge
      (:north (ecase next-edge
		(:north (coord 0 (- the-max the-col)))
		(:east (coord (- the-max the-col) the-max))
		(:south (coord the-max the-col))
		(:west (coord the-col 0))))
      (:south (ecase next-edge
		(:south (coord the-row (- the-max the-col)))
		(:east (coord the-col the-max))
		(:north (coord 0 the-col))
		(:west (coord (- the-max the-col) 0))))
      (:east (ecase next-edge
	       (:north (coord 0 (- the-max the-row)))
	       (:west (coord the-row 0))
	       (:south (coord the-max the-row))
	       (:east (coord (- the-max the-row) the-max))))
       (:west (ecase next-edge
		(:west (coord (- the-max the-row) 0))
		(:north (coord 0 the-row))
		(:east (coord the-row the-max))
		(:south (coord the-max (- the-max the-row))))))))

(defun test-next-sub ()
  (assert (coord= (coord 0 4) (next-sub 6 :north :north (coord 0 1))))
  (assert (coord= (coord 4 5) (next-sub 6 :north :east (coord 0 1))))
  )

(defun switch-mega (person)
  (let* ((next-side (gethash (side (mega-coord person) (compass person)) *connections*))
	 (next-mega-coord (side-coord next-side))
	 (next-border (side-compass next-side))
	 (next-sub (next-sub (square-size *mega-grid*) (compass person) next-border (sub-coord person))))
    (assert next-sub)
    (values next-mega-coord next-sub (opposite next-border))))

(defun next-position (person)
  (let ((try-sub-coord (coord+ (compass->move (compass person)) (sub-coord person))))
    (if (within-sub-bounds try-sub-coord)
	(values (mega-coord person) try-sub-coord (compass person))
	(switch-mega person))))

(defun turn-person (sym person)
  (make-person :mega-coord (mega-coord person) :sub-coord (sub-coord person)
	       :compass (rotate sym (compass person))))

(defun move-person (dist person)
  (loop with ret = person
	repeat dist
	do (multiple-value-bind (next-mega-coord next-sub next-compass) (next-position ret)
	     (setf ret (if (free-p next-mega-coord next-sub)
			   (make-person :mega-coord next-mega-coord :sub-coord next-sub :compass next-compass)
			   ret)))
	finally (return ret)))

(defun filled-p (coord)
  (let* ((inner (aref (grid *mega-grid*) (row coord) (col coord)))
	 (content (aref inner 0 0)))
    (or (char= #\# content) (char= #\. content))))

(defun set-val (mega sub val)
  (let ((outer (aref (grid *mega-grid*) (row mega) (col mega))))
    (setf (aref outer (row sub) (col sub)) val)))

(defun get-val (mega sub)
  (let ((outer (aref (grid *mega-grid*) (row mega) (col mega))))
    (aref outer (row sub) (col sub))))

(defun free-p (mega sub)
  (char= #\. (get-val mega sub)))

(defun wall-p (mega sub)
  (char= #\# (get-val mega sub)))

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
	 (*mega-grid* (make-mega-grid :original-rows rows :original-cols cols :square-size square-size
				      :mega-rows (/ rows square-size) :mega-cols (/ cols square-size)
				      :grid (make-array (list (/ rows square-size) (/ cols square-size))))))
    
    (loop for grid-row from 0 below (mega-rows *mega-grid*)
	  do (loop for grid-col from 0 below (mega-cols *mega-grid*)
		   do (setf (aref (grid *mega-grid*) grid-row grid-col)
			    (make-array (list square-size square-size) :initial-element nil))))

    (loop-all *mega-grid*
      (set-val (coord mega-row mega-col) (coord sub-row sub-col)
	       (if (< original-col (length (elt grid-lines original-row)))
		   (elt (elt grid-lines original-row) original-col)
		   '#\space)))
    
    *mega-grid*))

(defun load-file (square-size &optional (day "22"))
  (let* ((lines (read-day-file day)))
    (destructuring-bind (grid-lines direction-line) (parse-lines lines)
      (values (parse-directions direction-line) (grid-of-grids square-size grid-lines)))))

(defun p1-sample-connections ()
  (let ((dict (make-hash-table :test 'equal)))
    (add-connection dict (side (coord 0 2) :north) (side (coord 2 2) :south))
    (add-connection dict (side (coord 0 2) :east) (side (coord 0 2) :west))
    (add-connection dict (side (coord 0 2) :south) (side (coord 1 2) :north))

    (add-connection dict (side (coord 1 0) :north) (side (coord 1 0) :south))
    (add-connection dict (side (coord 1 0) :east) (side (coord 1 1) :west))
    (add-connection dict (side (coord 1 0) :west) (side (coord 1 2) :east))

    (add-connection dict (side (coord 1 1) :north) (side (coord 1 1) :south))
    (add-connection dict (side (coord 1 1) :east) (side (coord 1 2) :west))

    (add-connection dict (side (coord 1 2) :south) (side (coord 2 2) :north))

    (add-connection dict (side (coord 2 2) :east) (side (coord 2 3) :west))
    (add-connection dict (side (coord 2 2) :west) (side (coord 2 3) :east))
`
    (add-connection dict (side (coord 2 3) :north) (side (coord 2 3) :south))
    dict))

(defun p2-sample-connections ()
  (let ((dict (make-hash-table :test 'equal)))
    (add-connection dict (side (coord 0 2) :north) (side (coord 1 0) :north))
    (add-connection dict (side (coord 0 2) :east) (side (coord 2 3) :east))
    (add-connection dict (side (coord 0 2) :south) (side (coord 1 2) :north))
    (add-connection dict (side (coord 0 2) :west) (side (coord 1 1) :north))

    (add-connection dict (side (coord 1 0) :east) (side (coord 1 1) :west))
    (add-connection dict (side (coord 1 0) :west) (side (coord 2 3) :south))
    (add-connection dict (side (coord 1 0) :south) (side (coord 2 2) :south))

    (add-connection dict (side (coord 1 1) :east) (side (coord 1 2) :west))
    (add-connection dict (side (coord 1 1) :south) (side (coord 2 2) :west))

    (add-connection dict (side (coord 1 2) :east) (side (coord 2 3) :north))

    (add-connection dict (side (coord 2 2) :east) (side (coord 2 3) :west))
    dict))

(defun sample-1 ()
  (multiple-value-bind (instructions mega-grid) (load-file 4 "22s")
    (let ((*mega-grid* mega-grid)
	  (*connections* (p1-sample-connections))
	  (person (make-person :mega-coord (coord 0 2) :sub-coord (coord 0 0) :compass :east)))
      (loop for ins in instructions
	    do (progn (setf person (if (symbolp ins)
				       (turn-person ins person)
				       (move-person ins person)))
		      (format t "~A~%" person))
	    finally (return (score person))))))

(defun sample-2 ()
  (multiple-value-bind (instructions mega-grid) (load-file 4 "22s")
    (let ((*mega-grid* mega-grid)
	  (*connections* (p2-sample-connections))
	  (person (make-person :mega-coord (coord 0 2) :sub-coord (coord 0 0) :compass :east)))
      (loop for ins in instructions
	    do (progn (setf person (if (symbolp ins)
				       (turn-person ins person)
				       (move-person ins person)))
		      (format t "~A~%" person))
	    finally (return (score person))))))

(defun p1-connections ()
  (let ((dict (make-hash-table :test 'equal)))
    (add-connection dict (side (coord 0 1) :north) (side (coord 2 1) :south))
    (add-connection dict (side (coord 0 1) :east) (side (coord 0 2) :west))
    (add-connection dict (side (coord 0 1) :south) (side (coord 1 1) :north))
    (add-connection dict (side (coord 0 1) :west) (side (coord 0 2) :east))

    (add-connection dict (side (coord 0 2) :north) (side (coord 0 2) :south))

    (add-connection dict (side (coord 1 1) :east) (side (coord 1 1) :west))
    (add-connection dict (side (coord 1 1) :south) (side (coord 2 1) :north))

    (add-connection dict (side (coord 2 0) :north) (side (coord 3 0) :south))
    (add-connection dict (side (coord 2 0) :east) (side (coord 2 1) :west))
    (add-connection dict (side (coord 2 0) :west) (side (coord 2 1) :east))
    (add-connection dict (side (coord 2 0) :south) (side (coord 3 0) :north))

					; 2,1 is already connected
    (add-connection dict (side (coord 3 0) :east) (side (coord 3 0) :west))

    dict))

(defun p2-connections ()
  (let ((dict (make-hash-table :test 'equal)))
    (add-connection dict (side (coord 0 1) :north) (side (coord 3 0) :west))
    (add-connection dict (side (coord 0 1) :west) (side (coord 2 0) :west))
    (add-connection dict (side (coord 0 1) :south) (side (coord 1 1) :north))
    (add-connection dict (side (coord 0 1) :east) (side (coord 0 2) :west))

    (add-connection dict (side (coord 0 2) :north) (side (coord 3 0) :south))
    (add-connection dict (side (coord 0 2) :west) (side (coord 0 1) :east))
    (add-connection dict (side (coord 0 2) :south) (side (coord 1 1) :east))
    (add-connection dict (side (coord 0 2) :east) (side (coord 2 1) :east))

    (add-connection dict (side (coord 1 1) :north) (side (coord 0 1) :south))
    (add-connection dict (side (coord 1 1) :west) (side (coord 2 0) :north))
    (add-connection dict (side (coord 1 1) :east) (side (coord 0 2) :south))
    (add-connection dict (side (coord 1 1) :south) (side (coord 2 1) :north))

    (add-connection dict (side (coord 2 0) :north) (side (coord 1 1) :west))
    (add-connection dict (side (coord 2 0) :west) (side (coord 0 1) :west))
    (add-connection dict (side (coord 2 0) :east) (side (coord 2 1) :west))
    (add-connection dict (side (coord 2 0) :south) (side (coord 3 0) :north))

    (add-connection dict (side (coord 2 1) :north) (side (coord 1 1) :south))
    (add-connection dict (side (coord 2 1) :west) (side (coord 2 0) :east))
    (add-connection dict (side (coord 2 1) :east) (side (coord 0 2) :east))
    (add-connection dict (side (coord 2 1) :south) (side (coord 3 0) :east))

    (add-connection dict (side (coord 3 0) :north) (side (coord 2 0) :south))
    (add-connection dict (side (coord 3 0) :west) (side (coord 0 1) :north))
    (add-connection dict (side (coord 3 0) :east) (side (coord 2 1) :south))
    (add-connection dict (side (coord 3 0) :south) (side (coord 0 2) :north))
    dict))


(defun part-1 ()
  (multiple-value-bind (instructions mega-grid) (load-file 50)
    (let ((*mega-grid* mega-grid)
	  (*connections* (p1-connections))
	  (person (make-person :mega-coord (coord 0 1) :sub-coord (coord 0 0) :compass :east)))
      (loop for ins in instructions
	    do (progn (setf person (if (symbolp ins)
				       (turn-person ins person)
				       (move-person ins person)))
		      (format t "~A~%" person))
	    finally (return (score person))))))

(defun part-2 ()
  (multiple-value-bind (instructions mega-grid) (load-file 50)
    (let ((*mega-grid* mega-grid)
	  (*connections* (p2-connections))
	  (person (make-person :mega-coord (coord 0 1) :sub-coord (coord 0 0) :compass :east)))
      (loop for ins in instructions
	    do (progn (setf person (if (symbolp ins)
				       (turn-person ins person)
				       (move-person ins person)))
		      (format t "~A~%" person))
	    finally (return (score person))))))
