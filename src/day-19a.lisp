(defpackage :day-19a
  (:use :cl)
  (:import-from :utils :read-day-file :print-assert)
  (:import-from :alexandria :plist-hash-table)
  (:export #:exec))
(in-package :day-19a)

(defvar *blueprint* nil)
(defvar *max-ore* nil)
(defvar *max-clay* nil)
(defvar *max-obsidian* nil)
(defvar *max-elements* nil)
(defvar *num-days* 0)
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

(defun with-blueprint (blueprint num-days func)
  (let* ((*blueprint* blueprint)
	 (*num-days* num-days)
	 (*max-ore* (max (gethash :ore *blueprint*) (gethash :clay *blueprint*)
			 (car (gethash :obsidian *blueprint*)) (car (gethash :geode *blueprint*))))
	 (*max-clay* (cdr (gethash :obsidian *blueprint*)))
	 (*max-obsidian* (cdr (gethash :geode *blueprint*)))
	 (*max-elements* (list *max-ore* *max-clay* *max-obsidian* most-positive-fixnum)))
    (funcall func)))

(defmacro with-state-vars (the-state &body body)
  `(destructuring-bind (day (r-ore r-clay r-obsidian r-geode) (ore clay obsidian geode)) ,the-state
     ,@body))

(defun buy-nothing (state)
  (with-state-vars state
    (state (1+ day)
	   (elements r-ore r-clay r-obsidian r-geode)
	   (elements (+ ore r-ore) (+ clay r-clay) (+ obsidian r-obsidian) (+ geode r-geode)))))

(defun buy-ore (state)
  (let ((need (gethash :ore *blueprint*)))
    (with-state-vars state
      (if (and (<= need ore) (< r-ore *max-ore*))
	  (state (1+ day)
		 (elements (1+ r-ore) r-clay r-obsidian r-geode)
		 (elements (- (+ ore r-ore) need) (+ clay r-clay) (+ obsidian r-obsidian) (+ geode r-geode)))
	  nil))))

(defun buy-clay (state)
  (let ((need (gethash :clay *blueprint*)))
    (with-state-vars state
      (if (and (<= need ore) (< r-clay *max-clay*))
	  (state (1+ day)
		 (elements r-ore (1+ r-clay) r-obsidian r-geode)
		 (elements (- (+ ore r-ore) need) (+ clay r-clay) (+ obsidian r-obsidian) (+ geode r-geode)))
	  nil))))

(defun buy-obsidian (state)
  (destructuring-bind (need-ore . need-clay) (gethash :obsidian *blueprint*)
    (with-state-vars state
      (if (and (and (<= need-ore ore) (<= need-clay clay))
	       (< r-obsidian *max-obsidian*))
	  (state (1+ day)
		 (elements r-ore r-clay (1+ r-obsidian) r-geode)
		 (elements (- (+ ore r-ore) need-ore) (- (+ clay r-clay) need-clay) (+ obsidian r-obsidian) (+ geode r-geode)))
	  nil))))

(defun buy-geode (state)
  (destructuring-bind (need-ore . need-obsidian) (gethash :geode *blueprint*)
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

(defun key (state)
  (cons (day state) (mapcar #'min (robots state) *max-elements*)))

(defun prune-p (state)
  (let* ((current-geodes (geode state))
	 (key (key state))
	 (val (bank state))
	 (found (gethash key *seen*))
	 (max-possible-geodes (loop for i from current-geodes to (+ current-geodes (- *num-days* (day state)))
				    summing i)))

    (if (and found (elements<= val found))
	(return-from prune-p t))

    (setf (gethash key *seen*) val)
    (if (< max-possible-geodes *best-so-far*)
	t
	nil)))

(defun find-max ()
  (let ((*best-so-far* 0)
	(*seen* (make-hash-table :test #'equal)))
    (loop with stack = (list (state 0 (elements 1 0 0 0) (elements 0 0 0 0)))
	  while stack
	  do (let* ((current (pop stack))
		    (day (day current))
		    (geode (geode current)))
	       (if (< *best-so-far* geode)
		   (setf *best-so-far* geode))
	       
	       (if (and (< day *num-days*)
			(not (prune-p current)))
		   (loop for func in (list #'buy-nothing #'buy-ore #'buy-clay #'buy-obsidian #'buy-geode)
			 do (let ((next-state (funcall func current)))
			      (if next-state
				  (push next-state stack))))))
	  finally (return *best-so-far*))))
	      
(defun parse-line (line)
  (let ((numbers (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "[0-9]+" line))))
    (apply #'blueprint numbers)))

(defun part-1 (blueprints num-days)
  (loop for blueprint in blueprints
	summing (with-blueprint blueprint num-days
		  #'(lambda () (* (id blueprint) (find-max))))))

(defun part-2 (blueprints num-days)
  (apply #'* (loop for blueprint in (subseq blueprints 0 3)
		   collecting (with-blueprint blueprint num-days #'find-max))))

(defun exec ()
  (let ((blueprints (mapcar #'parse-line (read-day-file "19"))))
    (print-assert "Part 1:" (part-1 blueprints 24) 1115)
    (print-assert "Part 2:" (part-2 blueprints 32) 25056)))

