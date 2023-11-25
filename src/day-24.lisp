#|
This day looks fun?? Some ideas for solving this madness:

1) You don't really need to maintain all that state. Given the day and the initial position, you can always derive the current position of any given storm. It's just a combination of current day, original position, and a mod operation. This means that the entire state is just three values: current row, current column, and time step. In fact if you are really clever, you can severely limit the number of calculations by realizing that only a very small number of blizzards can affect the neighbor positions for any given day.

2) Neighbor calculation: See #1 above. In this case neighbor calculation will be any of north, south, east, west, stay or the nil case, and that means the particular path can just be dropped. This will probably be key to trimming the state.

3) A* seems feasible here. There's an obvious acceptable cost function, the manhattan distance. However, I don't know if A* can actually guarantee the best outcome since the grid is constantly changing (need to read more on A* to know for sure). If it can't then A* + BFS will give at least a plausible way to solve the problem by deprioritizing paths that are not making any advances. That plus playing all paths out until they are > than known solutions should guarantee an eventual solution.

4) Board states will repeat every row * col cycles as every blizzard will have returned to their original position. This should allow further trimming of board states that are the same (mod time (* row col)). It's definitely the case that if you are in a position, but there exists a board with fewer minutes in the same state, you can drop the one with more minutes, it's not going to make progress faster than the one with lower minutes count. This probably means that a fibonacci heap is the best data structure for tracking states. It gives the ability to inspect past states so worse solutions can just be dropped. In theory better solutions can replace current ones, but with this graph it's kind of hard to see if that's even possible to know.

5) Actually A* may just complicate things?? Don't know, I'm waffling.
|#
(defpackage :day-24
  (:use :cl)
  (:import-from :utils :read-day-file :print-assert)
  (:import-from :cl-heap :add-to-heap :decrease-key :pop-heap)
  (:import-from :alexandria :curry)
  (:export #:exec))
(in-package :day-24)
(declaim (optimize (debug 3)))
(defun state (minutes row col &optional (intermediates 0)) (vector minutes row col intermediates))
(defun minutes (state &optional new-val)
  (if new-val
      (setf (aref state 0) new-val)
      (aref state 0)))
(defun row (state) (aref state 1))
(defun col (state) (aref state 2))
(defun intermediates (state) (aref state 3))
(defun intermediates++ (state) (incf (aref state 3)))
(defun state+ (state move)
  (state (1+ (minutes state))
	 (+ (car move) (row state))
	 (+ (cdr move) (col state))
	 (intermediates state)))

(defvar *grid* nil)
(defvar *blizzard-height* 0)
(defvar *blizzard-width* 0)
(defvar *repeat-every* 0)
(defvar *index-cost-cache* nil)
(defvar *states* nil)
(defvar *best-so-far* nil)

(defparameter *possible-moves* (list (cons -1 0) (cons 1 0) (cons 0 -1) (cons 0 1) (cons 0 0)))

(defun state-key (state)
  (list (mod (minutes state) *repeat-every*) (row state) (col state) (intermediates state)))

(defun index-cost (state index) (cons index (minutes state)))
(defun index (ic) (car ic))
(defun cost (ic) (cdr ic))

(defun read-grid (day)
  (concatenate 'vector (read-day-file day)))

(defun at (row col) (aref (aref *grid* row) col))
(defun at-p (row col c) (char= c (at row col)))
(defun in-bounds-p (row col)
  (and (<= 0 row (1- (array-dimension *grid* 0)))
       (<= 0 col (1- (length (aref *grid* 0))))))

(defun wall-p (row col) (at-p row col #\#))
(defun free-p (row col) (at-p row col #\.))
(defun right-blizzard-p (row col) (at-p row col #\>))
(defun left-blizzard-p (row col) (at-p row col #\<))
(defun up-blizzard-p (row col) (at-p row col #\^))
(defun down-blizzard-p (row col) (at-p row col #\v))

(defun blizzard-char-p (c) (not (null (position c "v^<>1234"))))
(defun free-char-p (c) (char= c #\.))

(defun mod+ (f s m)
  (let ((tmp (+ f s)))
    (if (< tmp m) tmp (- tmp m))))

(defun mod- (f s m)
  (let ((tmp (- f s)))
    (if (<= 0 tmp) tmp (+ tmp m))))

(defun free-at-p (minutes row col)
  (if (or (and (= 0 row) (= 1 col))
	  (and (= (1+ *blizzard-height*) row) (= *blizzard-width* col)))
      (return-from free-at-p t))

  (let ((b-row (1- row))
	(b-col (1- col))
	(r-mod (mod minutes *blizzard-width*)))
	
    (if (or (right-blizzard-p row (1+ (mod- b-col r-mod *blizzard-width*)))
	    (left-blizzard-p row (1+ (mod+ b-col r-mod *blizzard-width*))))
	(return-from free-at-p nil))
    
    (let* ((c-mod (mod minutes *blizzard-height*)))
      (if (or (down-blizzard-p (1+ (mod- b-row c-mod *blizzard-height*)) col)
	      (up-blizzard-p (1+ (mod+ b-row c-mod *blizzard-height*)) col))
	  (return-from free-at-p nil))))
  t)

(defun legal-move-p (state)
  (let ((minutes (minutes state))
	(row (row state))
	(col (col state)))
    
    (and (in-bounds-p row col)
	 (not (wall-p row col))
	 (free-at-p minutes row col))))

(defun play-game (grid func)
  (let* ((*grid* grid)
	 (*blizzard-height* (- (length grid) 2))
	 (*blizzard-width* (- (length (aref grid 0)) 2))
	 (*repeat-every* (lcm *blizzard-height* *blizzard-width*))
	 (*index-cost-cache* (make-hash-table :test 'equal))
	 (*states* (make-instance 'cl-heap:fibonacci-heap :key #'minutes))
	 (*best-so-far* most-positive-fixnum))
    (funcall func)))

(defun add-state (state)
  (let ((key (state-key state)))
    (multiple-value-bind (ic present) (gethash key *index-cost-cache*)
      (cond ((not present)
	     (multiple-value-bind (item index) (add-to-heap *states* state)
	       (declare (ignore item))
	       (setf (gethash key *index-cost-cache*) (index-cost state index))))
	    
	    ((< (minutes state) (cost ic))
	     (decrease-key *states* (index ic) (minutes state))
	     (setf (gethash key *index-cost-cache*) (index-cost state (index ic))))))))
    
(defun solve (initial-state solved-p update)
  (add-state initial-state)
  (loop for current = (pop-heap *states*) then (pop-heap *states*)
	while current
	do (let ((cur-minutes (minutes current)))
	     (if (funcall solved-p current)
		 (setf *best-so-far* (min *best-so-far* cur-minutes))
		 (if (< cur-minutes *best-so-far*)
		     (loop for move in *possible-moves*
			   do (let ((possible (state+ current move)))
				(if (legal-move-p possible)
				    (add-state (funcall update possible))))))))
	finally (return *best-so-far*)))

(defun test-free-at-p (grid minutes)
    (loop for line across grid
	  for row from 0
	  do (loop for c across line
		   for col from 0
		   do (cond ((free-char-p c)
			     (assert (free-at-p minutes row col)))
			    ((blizzard-char-p c)
			     (assert (not (free-at-p minutes row col))))))))

(defun s-1 ()
  (format t "h: ~A w: ~A ev: ~A~%" *blizzard-height* *blizzard-width* *repeat-every*)
  (format t "dim ~A~%" (array-dimension *grid* 0))
  (assert (free-at-p 100 0 1))
  (assert (free-at-p most-positive-fixnum 5 6))
  (test-free-at-p *grid* 0)
  (test-free-at-p (vector "#.######" "#.>3.<.#" "#<..<<.#"
			  "#>2.22.#" "#>v..^<#" "######.#") 1)
  (test-free-at-p (vector "#.######" "#.2>2..#" "#.^22^<#"
			  "#.>2.^>#" "#.>..<.#" "######.#") 2)
  (test-free-at-p (vector "#.######" "#2.v.<>#" "#<.<..<#"
			  "#.^>^22#" "#.2..2.#" "######.#") 5)
  (test-free-at-p (vector "#.######" "#2^.^2>#" "#<v<.^<#"
			  "#..2.>2#" "#.<..>.#" "######.#") 11)
  )

(defun solved-1 (final-row final-col)
  (lambda (s)
    (and (= (row s) final-row) (= (col s) final-col))))

(defun sample-1 ()
  (play-game (read-grid "24s")
	     (curry #'solve (state 0 0 1) (solved-1 5 6) #'identity)))

(defun part-1 ()
  (play-game (read-grid "24")
	     (curry #'solve (state 0 0 1) (solved-1 36 100) #'identity)))

(defun solved-2 (final-row final-col)
  (lambda (s)
    (and (= (row s) final-row) (= (col s) final-col) (= 3 (intermediates s)))))

(defun set-intermediate (i-row i-col g-row g-col s)
  (if s
      (let ((row (row s))
	    (col (col s))
	    (intermediate (intermediates s)))
	(cond ((and (or (zerop intermediate) (= 2 intermediate))
		    (= row g-row)
		    (= col g-col))
	       (intermediates++ s))
	      
	      ((and (= 1 intermediate)
		    (= row i-row)
		    (= col i-col))
	       (intermediates++ s)))))
  s)

(defun sample-2 ()
  (play-game (read-grid "24s")
	     (curry #'solve
		    (state 0 0 1)
		    (solved-2 5 6)
		    (curry #'set-intermediate 0 1 5 6))))

(defun part-2 ()
  (play-game (read-grid "24")
	     (curry #'solve
		    (state 0 0 1)
		    (solved-2 36 100)
		    (curry #'set-intermediate 0 1 36 100))))

	      
  