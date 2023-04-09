(defpackage :day-14
  (:use :cl)
  (:import-from :utils :read-lists-of-symbols)
  (:import-from :alexandria :hash-table-alist)
  (:export #:part-1 #:part-2))

(declaim (optimize (debug 3)))

(in-package :day-14)

(defvar *cave* nil)
(defvar *max-y* nil)

(defun fill-in (cave x-start y-start x-end y-end)
  (if (= x-start x-end)
      (loop with x = x-start
            for y from (min y-start y-end) to (max y-start y-end)
            do (setf (gethash (cons x y) cave) t))
      (loop with y = y-start
            for x from (min x-start x-end) to (max x-start x-end)
            do (setf (gethash (cons x y) cave) t))))

(defun make-cave ()
  (let ((list-list-nums (read-lists-of-symbols "14")))
    (loop with cave = (make-hash-table :test #'equal)
          for list-nums in list-list-nums
          do (loop for nums on list-nums by #'cddr
                   do (if (third nums)
                          (fill-in cave (first nums) (second nums) (third nums) (fourth nums))))
          finally (return cave))))

(defun extract-max-y (cave)
  (loop with max-y = most-negative-fixnum
        for (x . y) being the hash-keys in cave
        do (setf max-y (max max-y y))
        finally (return max-y)))

(defun infinite-fall-p (coord)
  (<= *max-y* (cdr coord)))
                 
(defun drop-sands (move-func)
  (let* ((*cave* (make-cave))
         (*max-y* (extract-max-y *cave*)))
    (loop with counter = 0
          while (not (eq :finished (funcall move-func (cons 500 0))))
          do (incf counter)
          finally (return counter))))
    
(defun part-1-next-move (coord)
  (loop with status = :keep-going
        while (eq :keep-going status)
        do (let ((init-x (car coord))
                 (init-y (cdr coord)))
             (block try-it
               (setf (cdr coord) (1+ (cdr coord)))
               (if (not (gethash coord *cave*))
                   (progn
                     (if (infinite-fall-p coord)
                         (setf status :finished))
                     (return-from try-it)))

               (setf (car coord) (1- init-x))
               (if (not (gethash coord *cave*))
                   (return-from try-it))

               (setf (car coord) (1+ init-x))
               (if (not (gethash coord *cave*))
                   (return-from try-it))

               (setf (car coord) init-x)
               (setf (cdr coord) init-y)
               (setf (gethash coord *cave*) t)
               (setf status :at-rest)))
        finally (return status)))

(defun part-1 ()
  (drop-sands #'part-1-next-move))

(defun part-2-next-move (coord)
  (flet ((blocked ()
           (or (gethash coord *cave*)
               (= (+ 2 *max-y*) (cdr coord)))))
    (loop with status = :keep-going
          while (eq :keep-going status)
          do (let ((init-x (car coord))
                   (init-y (cdr coord)))
               (block try-it
                 (setf (cdr coord) (1+ (cdr coord)))
                 (if (not (blocked))
                     (return-from try-it))
                 
                 (setf (car coord) (1- init-x))
                 (if (not (blocked))
                     (return-from try-it))
                 
                 (setf (car coord) (1+ init-x))
                 (if (not (blocked))
                     (return-from try-it))
                 
                 (setf (car coord) init-x)
                 (setf (cdr coord) init-y)
                 (setf (gethash coord *cave*) t)
                 (setf status (if (equal '(500 . 0) coord) :finished :at-rest))))
          finally (return status))))

(defun part-2 ()
  (1+ (drop-sands #'part-2-next-move)))

