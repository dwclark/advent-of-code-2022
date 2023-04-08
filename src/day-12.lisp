(defpackage :day-12
  (:use :cl)
  (:import-from :utils :read-day-file :each-neighbor/4)
  (:import-from :cl-heap :decrease-key :fibonacci-heap :pop-heap :add-to-heap)
  (:export #:part-1 #:part-2))

(declaim (optimize (debug 3)))

(in-package :day-12)

(defvar *tracking* nil)
(defvar *frontier* nil)
(defvar *grid* nil)
(defvar *end-at* nil)

(defun load-grid ()
  (let* ((list-str (read-day-file "12"))
         (num-rows (length list-str))
         (num-cols (length (first list-str)))
         (grid (make-array (list num-rows num-cols) :initial-element 0))
         (start-at nil)
         (end-at nil))
    
    (loop for row in list-str
          for row-index from 0 below num-rows
          do (loop for char across row
                   for col-index from 0 below num-cols
                   do (cond ((char= char #\S)
                             (setf start-at (cons row-index col-index))
                             (setf (aref grid row-index col-index) 1))

                            ((char= char #\E)
                             (setf end-at (cons row-index col-index))
                             (setf (aref grid row-index col-index) 27))

                            (t (setf (aref grid row-index col-index) (1+ (- (char-code char) (char-code #\a)))))))
          finally (return (values grid start-at end-at)))))

(defun make-path (pos cost)
  (list pos cost nil))

(defun path-pos (path) (first path))

(defun (setf path-pos) (new-value path)
  (setf (first path) new-value))

(defun path-cost (path) (second path))

(defun (setf path-cost) (new-value path)
  (setf (second path) new-value))

(defun path-node (path) (third path))

(defun (setf path-node) (new-value path)
  (setf (third path) new-value))

(defun end-p (path)
  (equal (path-pos path) *end-at*))

(defun track-position (pos cost)
  (if (or (null pos) (null cost))
      (break))
  (let ((tracked (gethash pos *tracking*)))
    (if tracked
        (if (< cost (path-cost tracked))
            (decrease-key *frontier* (path-node tracked) cost))
        (let* ((to-track (make-path pos cost))
               (node (second (multiple-value-list (add-to-heap *frontier* to-track)))))
          (setf (path-node to-track) node)
          (setf (gethash pos *tracking*) to-track)))))

(defun find-shortest (start-at)
  (track-position start-at 0)
  (loop for path = (pop-heap *frontier*) then (pop-heap *frontier*)
        while (and (not (null path)) (not (end-p path)))
        do (let* ((pos (path-pos path))
                  (cost (path-cost path))
                  (height (aref *grid* (car pos) (cdr pos))))
             (each-neighbor/4 *grid* (car pos) (cdr pos)
                              (lambda (new-r new-c)
                                (let* ((new-height (aref *grid* new-r new-c))
                                       (new-pos (cons new-r new-c)))
                                  (if (<= (1- new-height) height)
                                      (track-position new-pos (1+ cost)))))))
        finally (return (if (null path) most-positive-fixnum (path-cost path)))))

(defun find-shortest-no-key-change (start-at)
  (setf (gethash start-at *tracking*) t)
  (add-to-heap *frontier* (make-path start-at 0))
  (loop for path = (pop-heap *frontier*) then (pop-heap *frontier*)
        while (and (not (null path)) (not (end-p path)))
        do (let* ((pos (path-pos path))
                  (cost (path-cost path))
                  (height (aref *grid* (car pos) (cdr pos))))
             (each-neighbor/4 *grid* (car pos) (cdr pos)
                              (lambda (neighbor-row neighbor-col)
                                (let* ((new-height (aref *grid* neighbor-row neighbor-col))
                                       (neighbor-pos (cons neighbor-row neighbor-col)))
                                  (if (and (<= (1- new-height) height)
                                           (not (gethash neighbor-pos *tracking*)))
                                      (progn
                                        (setf (gethash neighbor-pos *tracking*) t)
                                        (add-to-heap *frontier* (make-path neighbor-pos (1+ cost)))))))))
        finally (return (if (null path) most-positive-fixnum (path-cost path)))))


(defun part-1 ()
  (multiple-value-bind (*grid* start-at *end-at*) (load-grid)
    (let ((*tracking* (make-hash-table :test #'equal))
          (*frontier* (make-instance 'fibonacci-heap :key #'path-cost)))
      (find-shortest start-at))))

(defun possible-starts ()
  (loop with ret = nil
        for row from 0 below (array-dimension *grid* 0)
        do (loop for col from 0 below (array-dimension *grid* 1)
                 do (if (= 1 (aref *grid* row col))
                        (push (cons row col) ret)))
        finally (return (nreverse ret))))
                                                       
(defun part-2 ()
  (multiple-value-bind (*grid* tmp *end-at*) (load-grid)
    (declare (ignore tmp))
    (loop with shortest = most-positive-fixnum
          for start-at in (possible-starts)
          do (let ((*tracking* (make-hash-table :test #'equal))
                   (*frontier* (make-instance 'fibonacci-heap :key #'path-cost)))
               (setf shortest (min shortest (find-shortest start-at))))
          finally (return shortest))))

(defun part-1-no-key-change ()
  (multiple-value-bind (*grid* start-at *end-at*) (load-grid)
    (let ((*tracking* (make-hash-table :test #'equal))
          (*frontier* (make-instance 'fibonacci-heap :key #'path-cost)))
      (find-shortest-no-key-change start-at))))

(defun part-2-no-key-change ()
  (multiple-value-bind (*grid* tmp *end-at*) (load-grid)
    (declare (ignore tmp))
    (loop with shortest = most-positive-fixnum
          for start-at in (possible-starts)
          do (let ((*tracking* (make-hash-table :test #'equal))
                   (*frontier* (make-instance 'fibonacci-heap :key #'path-cost)))
               (setf shortest (min shortest (find-shortest-no-key-change start-at))))
          finally (return shortest))))
