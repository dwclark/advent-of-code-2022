(defpackage :day-17
  (:use :cl)
  (:import-from :utils :read-day-file :print-assert)
  (:import-from :alexandria :copy-array :curry)
  (:export #:exec))
(in-package :day-17)

(defparameter *bottom* "---------")
(defparameter *blank-line* "|.......|")

(defun new-rocks ()
  (vector (vector 1 4 (vector "####"))
	  (vector 3 3 (vector ".#." "###" ".#."))
	  (vector 3 3 (vector "..#" "..#" "###"))
	  (vector 4 1 (vector "#" "#" "#" "#"))
	  (vector 2 2 (vector "##" "##"))))

(defun new-column (vec)
  (let ((ret (make-array 1 :adjustable t :fill-pointer 0)))
    (loop for line across (reverse vec)
	  do (vector-push-extend (copy-array line) ret))
    ret))

(defvar *rocks* nil)
(defvar *column* nil)
(defvar *jets* nil)

(defun new-iter (vec)
  (let ((counter 0))
    (lambda (&key (peek nil))
      (let ((index (mod counter (length vec))))
	(if (not peek)
	    (incf counter))
	(values (aref vec index) index)))))

(defun rock-at-p (row col)
 (rock-p (aref (aref *column* row) col)))

(defun height (r) (aref r 0))
(defun width  (r) (aref r 1))
(defun contents (r) (aref r 2))
(defun contents-at (r row col)
  (aref (aref (contents r) row) col)) 
(defun needed-height (r) (+ 3 (height r)))

(defun parse-jets (day)
  (labels ((translate (c)
	     (assert (or (char= #\< c) (char= #\>)))
	     (if (char= #\< c) :left :right)))
    (map 'vector #'translate (first (read-day-file day)))))

(defun rock-p (c)
  (not (null (position c "#|-"))))

(defun has-rocks-p (line)
  (position-if #'rock-p line :start 1 :end 8))

(defun top-rock-line ()
  (loop for index from (1- (length *column*)) downto 0
	do (if (has-rocks-p (aref *column* index))
	       (return-from top-rock-line index))))

(defun fresh-lines-needed (r)
  (let* ((top (top-rock-line))
	 (num-blanks (- (length *column*) top 1)))
    (- (+ 3 (height r)) num-blanks)))

(defun fill-fresh-lines (need)
  (loop repeat need do (vector-push-extend (copy-array *blank-line*) *column*)))

(defun iter-rock (rock op)
  (loop for row from 0 below (height rock)
	do (loop for col from 0 below (width rock)
		 do (when (and (rock-p (contents-at rock row col))
			       (funcall op row col))
		      (return-from iter-rock :stop)))
	finally (return :continue)))

(defun move-left-p (rock grid-row grid-col)
  (flet ((test (row col)
	   (rock-at-p (- grid-row row)
		      (1- (+ grid-col col)))))
    (iter-rock rock #'test)))

(defun move-right-p (rock grid-row grid-col)
  (flet ((test (row col)
	   (rock-at-p (- grid-row row)
		      (1+ (+ grid-col col)))))
    (iter-rock rock #'test)))
  
(defun move-down-p (rock grid-row grid-col)
  (flet ((test (row col)
	   (rock-at-p (1- (- grid-row row))
		      (+ grid-col col))))
    (iter-rock rock #'test)))

(defun copy-in-place (rock grid-row grid-col)
  (flet ((copy (row col)
	   (setf (aref (aref *column* (- grid-row row)) (+ grid-col col))
		 (contents-at rock row col))
	   nil))
    (iter-rock rock #'copy)))

(defun move-rock-round (rock jet row col)
  (ecase jet
    (:left (if (eq :continue (move-left-p rock row col))
	       (decf col)))
    (:right (if (eq :continue (move-right-p rock row col))
		(incf col))))
  
  (ecase (move-down-p rock row col)
    (:continue (values :continue (decf row) col))
    (:stop
     (copy-in-place rock row col)
     (values :stop row col))))

(defun move-rock (rock-iter jet-iter)
  (multiple-value-bind (rock rock-index) (funcall rock-iter)
    (loop with next-op = :continue
	  with row = (+ (top-rock-line) (height rock) 3)
	  with col = 3
	  while (eq :continue next-op)
	  do (multiple-value-bind (jet jet-index) (funcall jet-iter)
	       (multiple-value-setq (next-op row col) (move-rock-round rock jet row col))))))

(defun run-game (day func)
  (let ((*rocks* (new-rocks))
	(*column* (new-column (vector *bottom*)))
	(*jets* (parse-jets day)))
    (funcall func)))

(defun print-game ()
  (loop for index from (1- (length *column*)) downto 0
	do (format t "~A~%" (aref *column* index))))

(defun game-loop (times)
  (loop with rock-iter = (new-iter *rocks*)
	with jet-iter = (new-iter *jets*)
	repeat times
	do (let ((rock (funcall rock-iter :peek t)))
	     (fill-fresh-lines (fresh-lines-needed rock))
	     (move-rock rock-iter jet-iter))
	finally (return (top-rock-line))))

(defun sample-1 (times)
  (run-game "17s" (lambda ()
		    (game-loop times)
		    (top-rock-line))))

(defun part-1 ()
  (run-game "17" (curry #'game-loop 2022)))

(defun test-extreme ()
  (let ((rock (vector 3 3 (vector ".#." "###" ".#.")))
	(*column* (new-column (vector "|.......|"
				      "|##.....|"
				      "|#......|"
				      "|##.....|"
				      "---------"))))
    (multiple-value-bind (action row col) (move-rock-round rock :left 3 3)
      (assert (eq :stop action))
      (assert (= 3 row))
      (assert (= 2 col))
      (print-game))))

