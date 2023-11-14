(defpackage :day-19a
  (:use :cl)
  (:import-from :utils :read-day-file :print-assert)
  (:import-from :alexandria :plist-hash-table)
  (:export #:exec))
(in-package :day-19a)

(defvar *seen* nil)
(defvar *best-so-far* nil)

(defun elements (ore clay obsidian geode)
  (list ore clay obsidian geode))

(defun elements+ (m1 m2) (mapcar #'+ m1 m2))
(defun elements<= (m1 m2) (every #'<= m1 m2))
(defun elements< (m1 m2) (every #'< m1 m2))

(defun blueprint (id o4-ore o4-clay o4-obs c4-obs o4-geo obs4-geo)
  (plist-hash-table (list :id id :ore o4-ore :clay o4-clay
			  :obsidian (cons o4-obs c4-obs)
			  :geode (cons o4-geo obs4-geo))))

(defmacro with-state-vars (the-state &body body)
  `(destructuring-bind (day (r-ore r-clay r-obsidian r-geode) (ore clay obsidian geode)) ,the-state
     ,@body))

(defun buy-nothing (blueprint state)
  (declare (ignore blueprint))
  (with-state-vars state
    (state (1+ day)
	   (elements r-ore r-clay r-obsidian r-geode)
	   (elements (+ ore r-ore) (+ clay r-clay) (+ obsidian r-obsidian) (+ geode r-geode)))))

(defun buy-ore (blueprint state)
  (let ((need (gethash :ore blueprint)))
    (with-state-vars state
      (if (<= need ore)
	  (state (1+ day)
		 (elements (1+ r-ore) r-clay r-obsidian r-geode)
		 (elements (- (+ ore r-ore) need) (+ clay r-clay) (+ obsidian r-obsidian) (+ geode r-geode)))
	  nil))))

(defun buy-clay (blueprint state)
  (let ((need (gethash :clay blueprint)))
    (with-state-vars state
      (if (<= need ore)
	  (state (1+ day)
		 (elements r-ore (1+ r-clay) r-obsidian r-geode)
		 (elements (- (+ ore r-ore) need) (+ clay r-clay) (+ obsidian r-obsidian) (+ geode r-geode)))
	  nil))))

(defun buy-obsidian (blueprint state)
  (destructuring-bind (need-ore . need-clay) (gethash :obsidian blueprint)
    (with-state-vars state
      (if (and (<= need-ore ore) (<= need-clay clay))
	  (state (1+ day)
		 (elements r-ore r-clay (1+ r-obsidian) r-geode)
		 (elements (- (+ ore r-ore) need-ore) (- (+ clay r-clay) need-clay) (+ obsidian r-obsidian) (+ geode r-geode)))
	  nil))))

(defun buy-geode (blueprint state)
  (destructuring-bind (need-ore . need-obsidian) (gethash :geode blueprint)
    (with-state-vars state
      (if (and (<= need-ore ore) (<= need-obsidian obsidian))
	  (state (1+ day)
		 (elements r-ore r-clay r-obsidian (1+ r-geode))
		 (elements (- (+ ore r-ore) need-ore) (+ clay r-clay) (- (+ obsidian r-obsidian) need-obsidian) (+ geode r-geode)))
	  nil))))

(defun id (b) (gethash :id b))

(defun state (day robots bank)
  (list day robots bank))

(defun day (state) (first state))
(defun robots (state) (second state))
(defun bank (state) (third state))

(defun geode (o)
  (let ((tmp (car (last o))))
    (if (atom tmp)
	tmp
	(car (last tmp))))) 

(defun key (state) (cons (day state) (robots state)))

(defun prune-p (state)
  (let* ((current-geodes (geode state))
	 (key (key state))
	 (val (bank state))
	 (found (gethash key *seen*))
	 (max-possible-geodes (loop for i from current-geodes to (+ current-geodes (- 24 (day state)))
				    summing i)))

    (if (and found (elements<= val found))
	(return-from prune-p t))

    (setf (gethash key *seen*) val)
    (if (< max-possible-geodes *best-so-far*)
	t
	nil)))

(defun test ()
  (assert (equal (state 25 (elements 1 4 2 2) (elements 6 41 8 9))
		 (buy-nothing nil (state 24 (elements 1 4 2 2) (elements 5 37 6 7)))))
  (assert (equal (buy-geode (blueprint 1 4 2 3 14 2 7)
			    (state 21 (elements 1 4 2 1) (elements 4 25 7 2)))
		 (state 22 (elements 1 4 2 2) (elements 3 29 2 3)))) 

  )
  

(defun find-max (blueprint)
  (let ((*best-so-far* 0)
	(*seen* (make-hash-table :test #'equal)))
    (loop with stack = (list (state 0 (elements 1 0 0 0) (elements 0 0 0 0)))
	  while stack
	  do (let* ((current (pop stack))
		    (day (day current))
		    (geode (geode current)))
	       (if (< *best-so-far* geode)
		   (setf *best-so-far* geode))
	       
	       (if (and (< day 24)
			(not (prune-p current)))
		   (loop for func in (list #'buy-nothing #'buy-ore #'buy-clay #'buy-obsidian #'buy-geode)
			 do (let ((next-state (funcall func blueprint current)))
			      (if next-state
				  (push next-state stack))))))
	  finally (return *best-so-far*))))
	      
(defun parse-line (line)
  (let ((numbers (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "[0-9]+" line))))
    (apply #'blueprint numbers)))

(defun exec ()
  (loop for blueprint in (mapcar #'parse-line (read-day-file "19"))
	summing (* (id blueprint) (find-max blueprint))))
