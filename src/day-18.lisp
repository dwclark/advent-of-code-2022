(defpackage :day-18
  (:use :cl)
  (:import-from :utils :read-day-file :print-assert)
  (:import-from :alexandria :hash-table-keys)
  (:import-from :aops :each)
  (:import-from :split-sequence :split-sequence)
  (:export #:exec))
(in-package :day-18)

(defvar *map-cubes* nil)
(defvar *neighbor-transforms* (list (vector -1 0 0) (vector 1 0 0)
				    (vector 0 -1 0) (vector 0 1 0)
				    (vector 0 0 -1) (vector 0 0 1)))

(defun to-neighbors (cube)
  (labels ((add (v) (each #'+ cube v)))
    (mapcar #'add *neighbor-transforms*)))

(defun cube-at-p (cube) (gethash cube *map-cubes*))
(defun air-at-p (cube) (not (cube-at-p cube)))

(defun neighbors-matching (cube criteria)
  (loop for neighbor in (to-neighbors cube)
	summing (if (funcall criteria neighbor) 1 0)))

(defun surface-count (list-cubes criteria)
  (loop for cube in list-cubes
	summing (neighbors-matching cube criteria)))

(defun cubes-limits (list-cubes)
  (loop for cube in list-cubes
	minimizing (aref cube 0) into min-x maximizing (aref cube 0) into max-x
	minimizing (aref cube 1) into min-y maximizing (aref cube 1) into max-y
	minimizing (aref cube 2) into min-z maximizing (aref cube 2) into max-z
	finally (return (list min-x max-y min-y max-y min-z max-z))))

(defun beyond-limits-p (cube limits)
  (destructuring-bind (min-x max-x min-y max-y min-z max-z) limits
      (let ((x (aref cube 0))
	    (y (aref cube 1))
	    (z (aref cube 2)))
	(or (< x min-x) (< max-x x)
	    (< y min-y) (< max-y y)
	    (< z min-z) (< max-z z)))))

(defun air-pocket-p (cube limits)
  (if (cube-at-p cube)
      (return-from air-pocket-p nil))
  (let ((to-visit nil)
	(visited (make-hash-table :test #'equalp)))
    (labels ((add-to-visit (n)
	       (if (and (air-at-p n) (not (gethash n visited)))
		   (push n to-visit)))

	     (add-visited (n)
	       (setf (gethash n visited) t))

	     (complete-visit (n)
	       (add-visited n)
	       (dolist (next (to-neighbors n))
		 (add-to-visit next))))
      
      (complete-visit cube)

      (loop while to-visit
	    do (let ((next (pop to-visit)))
		 (if (beyond-limits-p next limits)
		     (return-from air-pocket-p nil)
		     (complete-visit next)))
	    finally (return t)))))

(defun air-pockets (limits)
  (destructuring-bind (min-x max-x min-y max-y min-z max-z) limits
    (loop with ret = (make-hash-table :test #'equalp)
	  for x from min-x to max-x
	  do (loop for y from min-y to max-y
		   do (loop for z from min-z to max-z
			    do (let ((cube (vector x y z)))
				 (if (air-pocket-p cube limits)
				     (setf (gethash cube ret) t)))))
	  finally (return ret))))

;; TODO: implement flood fill solution, looks simpler and faster
(defun exec ()
  (let* ((list-cubes (mapcar #'(lambda (line)
				 (map 'vector #'parse-integer (split-sequence #\, line :test #'char=))) (read-day-file "18")))
	 (*map-cubes* (loop with ret = (make-hash-table :test #'equalp)
			    for vec in list-cubes
			    do (setf (gethash vec ret) t)
			    finally (return ret)))
	 (full-surface-count (surface-count list-cubes #'air-at-p))
	 (limits (cubes-limits list-cubes))
	 (list-pockets (hash-table-keys (air-pockets limits)))
	 (pocket-surface-count (surface-count list-pockets #'cube-at-p)))
    
    (print-assert "Part 1:" full-surface-count 4474)
    (print-assert "Part 2:" (- full-surface-count pocket-surface-count) 2518)))
