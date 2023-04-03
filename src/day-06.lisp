(defpackage :day-06
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:export #:part-1 #:part-2))

(in-package :day-06)

(defun find-marker (width)
  (loop with message = (first (read-day-file "06"))
        for i from 0 below (- (length message) width)
        do (if (= width (length (remove-duplicates (subseq message i (+ i width)) :test #'char=)))
               (return (+ width i)))))

(defun part-1 ()
  (find-marker 4))

(defun part-2 ()
  (find-marker 14))
