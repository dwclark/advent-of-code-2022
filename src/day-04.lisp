(defpackage :day-04
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :do-register-groups)
  (:export #:part-1 #:part-2))

(in-package :day-04)

(defun parse-line (line)
  (do-register-groups ((#'parse-integer i1 i2 i3 i4)) ("^(\\d+)-(\\d+),(\\d+)-(\\d+)$" line)
    (return (vector (cons i1 i2) (cons i3 i4)))))

(defun contained-p (v)
  (flet ((test (one two)
           (and (<= (car one) (car two))
                (<= (cdr two) (cdr one)))))
    (or (test (aref v 0) (aref v 1))
        (test (aref v 1) (aref v 0)))))

(defun overlap-p (v)
  (flet ((test (one two)
           (or (<= (car one) (car two) (cdr one))
               (<= (car one) (cdr two) (cdr one)))))
    (or (test (aref v 0) (aref v 1))
        (test (aref v 1) (aref v 0)))))
           
(defun part-1 ()
  (count-if #'contained-p (mapcar #'parse-line (read-day-file "04"))))

(defun part-2 ()
  (count-if #'overlap-p (mapcar #'parse-line (read-day-file "04"))))
