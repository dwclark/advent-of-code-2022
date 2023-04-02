(defpackage :day-03
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :alexandria :multiple-value-compose)
  (:export #:part-1 #:part-2))

(in-package :day-03)

(defun char->priority (c)
  (if (lower-case-p c)
      (1+ (- (char-code c) (char-code #\a)))
      (+ 27 (- (char-code c) (char-code #\A)))))

(defun part-1 ()
  (flet ((find-duplicate (str)
           (loop with mid = (/ (length str) 2)
                 for c1 across (subseq str 0 mid)
                 do (if (find c1 str :start mid :test #'char=)
                        (return-from find-duplicate c1)))))
    (reduce #'+ (mapcar (multiple-value-compose #'char->priority #'find-duplicate) (read-day-file "03")))))

(defun part-2 ()
  (flet ((find-duplicates (lst)
           (loop for sub on lst by #'cdddr
                 for one = (first sub) then (first sub)
                 for two = (second sub) then (second sub)
                 for three = (third sub) then (third sub)
                 collecting (loop for c across one
                                  do (if (and (find c two :test #'char=)
                                              (find c three :test #'char=))
                                         (return c))) into duplicates
                 finally (return duplicates))))
    (reduce #'+ (mapcar #'char->priority (find-duplicates (read-day-file "03"))))))
                                               
