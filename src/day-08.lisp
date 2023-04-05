(defpackage :day-08
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :rcurry)
  (:export #:part-1 #:part-2))

(in-package :day-08)

(defvar *board* nil)

(defun load-grid ()
  (let* ((raw (read-day-file "08"))
         (ret (make-array (list (length raw) (length (first raw))) :element-type 'fixnum)))
    (loop for row in raw
          for row-index from 0 below (length raw)
          do (loop for col-index from 0 below (length row)
                   do (setf (aref ret row-index col-index) (parse-integer (format nil "~A" (aref row col-index)))))
          finally (return ret))))

(defun top-visible (row col)
  (loop with num = (aref *board* row col)
        for row-index from (1- row) downto 0
        counting 1 into visited
        do (if (<= num (aref *board* row-index col))
               (return (values nil visited)))
        finally (return (values t (if visited visited 0)))))

(defun left-visible (row col)
  (loop with num = (aref *board* row col)
        for col-index from (1- col) downto 0
        counting 1 into visited
        do (if (<= num (aref *board* row col-index))
               (return (values nil visited)))
        finally (return (values t (if visited visited 0)))))

(defun bottom-visible (row col)
  (loop with num = (aref *board* row col)
        for row-index from (1+ row) below (array-dimension *board* 0)
        counting 1 into visited
        do (if (<= num (aref *board* row-index col))
               (return (values nil visited)))
        finally (return (values t (if visited visited 0)))))

(defun right-visible (row col)
  (loop with num = (aref *board* row col)
        for col-index from (1+ col) below (array-dimension *board* 1)
        counting 1 into visited
        do (if (<= num (aref *board* row col-index))
               (return (values nil visited)))
        finally (return (values t (if visited visited 0)))))

(defun part-1 ()
  (let ((*board* (load-grid)))
    (loop with total = 0
          for row from 0 below (array-dimension *board* 0)
          do (loop for col from 0 below (array-dimension *board* 1)
                   do (incf total (if (or (top-visible row col)
                                          (left-visible row col)
                                          (bottom-visible row col)
                                          (right-visible row col))
                                      1
                                      0)))
          finally (return total))))

(defun part-2 ()
  (let ((*board* (load-grid)))
    (loop for row from 0 below (array-dimension *board* 0)
          maximizing (loop for col from 0 below (array-dimension *board* 1)
                           maximizing (* (second (multiple-value-list (top-visible row col)))
                                         (second (multiple-value-list (left-visible row col)))
                                         (second (multiple-value-list (bottom-visible row col)))
                                         (second (multiple-value-list (right-visible row col)))) into the-max
                           finally (return the-max)) into final-max
          finally (return final-max))))
