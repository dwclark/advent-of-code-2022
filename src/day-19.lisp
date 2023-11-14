(defpackage :day-19
  (:use :cl)
  (:import-from :utils :read-day-file :print-assert)
  (:export #:exec))
(in-package :day-19)

(defstruct elements (ore 0 :type fixnum) (clay 0 :type fixnum) (obsidian 0 :type fixnum) (geode 0 :type fixnum))

(defstruct blueprint
  (id 0 :type fixnum)
  (ore nil :type elements)
  (clay nil :type elements)
  (obsidian nil :type elements)
  (geode nil :type elements))

(defstruct (state (:copier nil))
  (day 0 :type fixnum)
  (ores (make-elements) :type elements)
  (robots (make-elements :ore 1) :type elements))

(defun copy-state (s)
  (make-state :day (state-day s) :ores (copy-elements (state-ores s)) :robots (copy-elements (state-robots s))))

(defvar *blueprint* nil)
(defvar *best-so-far* 0)
(defvar *seen* nil)
(defvar *pruned* (make-state :day 25 :ores (make-elements) :robots (make-elements)))

(defun can-buy-p (need have)
  (and (<= (elements-ore need) (elements-ore have))
       (<= (elements-clay need) (elements-clay have))
       (<= (elements-obsidian need) (elements-obsidian have))))

(defun buy (cost state-ores)
  (decf (elements-ore state-ores) (elements-ore cost))
  (decf (elements-clay state-ores) (elements-clay cost))
  (decf (elements-obsidian state-ores) (elements-obsidian cost)))

(defun earn (state)
  (let ((ores (state-ores state))
	(robots (state-robots state)))
    (incf (elements-ore ores) (elements-ore robots))
    (incf (elements-clay ores) (elements-clay robots))
    (incf (elements-obsidian ores) (elements-obsidian robots))
    (incf (elements-geode ores) (elements-geode robots))
    (incf (state-day state))
    state))

(defun day-list (state)
  ;earn
  (let ((ret (list (earn (copy-state state)))))
    
    ; buy ore machine
    (if (can-buy-p (blueprint-ore *blueprint*) (state-ores state))
	(let ((new-state (earn (copy-state state))))
	  (buy (blueprint-ore *blueprint*) (state-ores new-state))
	  (incf (elements-ore (state-robots new-state)))
	  (push new-state ret)))

    ;buy clay machine
    (if (can-buy-p (blueprint-clay *blueprint*) (state-ores state))
	(let ((new-state (earn (copy-state state))))
	  (buy (blueprint-clay *blueprint*) (state-ores new-state))
	  (incf (elements-clay (state-robots new-state)))
	  (push new-state ret)))

    ; buy obsidian machine
    (if (can-buy-p (blueprint-obsidian *blueprint*) (state-ores state))
	(let ((new-state (earn (copy-state state))))
	  (buy (blueprint-obsidian *blueprint*) (state-ores new-state))
	  (incf (elements-obsidian (state-robots new-state)))
	  (push new-state ret)))

    ; buy geode machine
    (if (can-buy-p (blueprint-geode *blueprint*) (state-ores state))
	(let ((new-state (earn (copy-state state))))
	  (buy (blueprint-geode *blueprint*) (state-ores new-state))
	  (incf (elements-geode (state-robots new-state)))
	  (push new-state ret)))
    
    ret))

(defun max-geode (s1 s2)
  (if (< (elements-geode (state-ores s1)) (elements-geode (state-ores s2)))
      s2
      s1))

(defun max-possible-geodes (state)
  (let ((days-left (- 24 (state-day state)))
	(geodes (elements-geode (state-ores state))))
    (+ geodes (/ (* days-left (1+ days-left)) 2))))

(defun prune-p (state)
  (let* ((ores (state-ores state))
	 (robots (state-robots state))
	 (key (list (state-day state) (elements-ore ores) (elements-clay ores) (elements-obsidian ores) (elements-geode ores)))
	 (prev (gethash key *seen*)))
    
    (if (and prev (and (<= (elements-ore robots) (elements-ore prev))
		       (<= (elements-clay robots) (elements-clay prev))
		       (<= (elements-obsidian robots) (elements-obsidian prev))
		       (<= (elements-geode robots) (elements-geode prev))))
	(return-from prune-p t))
    
    (setf (gethash key *seen*) robots)
    
    (if (< (max-possible-geodes state) *best-so-far*)
	(return-from prune-p t))
    
    nil))
  
(defun find-best (state)
  (cond ((= 25 (state-day state))
	 (let ((my-geode (elements-geode (state-robots state))))
	   (if (< *best-so-far* my-geode)
	       (progn
		 (setf *best-so-far* my-geode)
		 (format t "Improved geode score to ~A~%" my-geode))))
	 state)

	((prune-p state) *pruned*)
	
	(t (reduce #'max-geode (mapcar #'find-best (day-list state))))))

(defun parse-line (line)
  (let ((numbers (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "[0-9]+" line))))
    (make-blueprint :id (elt numbers 0)
		    :ore (make-elements :ore (elt numbers 1))
		    :clay (make-elements :ore (elt numbers 2))
		    :obsidian (make-elements :ore (elt numbers 3) :clay (elt numbers 4))
		    :geode (make-elements :ore (elt numbers 5) :obsidian (elt numbers 6)))))

(defun exec ()
  (let ((all-blueprints (mapcar #'parse-line (read-day-file "19"))))
    (loop for blueprint in all-blueprints
	  do (let ((*blueprint* blueprint)
		   (*best-so-far* 0)
		   (*seen* (make-hash-table :test #'equal)))
	       ;(format t "~A~%" blueprint)))))
	       (format t "~A ~A~%" (blueprint-id blueprint) (find-best (make-state)))))))
