(defpackage :day-01
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :split-sequence :split-sequence)
  (:export #:part-1 #:part-2))

(in-package :day-01)

(defun ordered-sums ()
  (let ((grouped-lists (split-sequence "" (read-day-file "01") :test #'string=)))
    (flet ((sum-list (lst)
             (reduce #'+ (mapcar #'parse-integer lst))))
      (sort (mapcar #'sum-list grouped-lists)  #'>))))

(defun part-1 ()
  (first (ordered-sums)))

(defun part-2 ()
  (reduce #'+ (subseq (ordered-sums) 0 3)))
