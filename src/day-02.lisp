(defpackage :day-02
  (:use :cl)
  (:import-from :utils :read-lists-of-symbols)
  (:import-from :alexandria :plist-hash-table)
  (:export #:part-1 #:part-2))

(in-package :day-02)

(defparameter *inputs* nil)
(defparameter +rock+ 'a)
(defparameter +paper+ 'b)
(defparameter +scissors+ 'c)
(defparameter *translation* (plist-hash-table (list 'x +rock+ 'y +paper+ 'z +scissors+)))
(defparameter *scores* (plist-hash-table (list +rock+ 1 +paper+ 2 +scissors+ 3
                                               :lose 0 :draw 3 :win 6)))

(defun score (p1 p2)
  (let ((outcome (cond ((eq p1 p2) :draw)
                       ((or (and (eq +rock+ p1) (eq +scissors+ p2))
                            (and (eq +paper+ p1) (eq +rock+ p2))
                            (and (eq +scissors+ p1) (eq +paper+ p2))) :lose)
                       (t :win))))
    (values (+ (gethash outcome *scores*) (gethash p2 *scores*)) outcome)))

(defun part-1 ()
  (let ((*inputs* (read-lists-of-symbols "02")))
    (reduce #'+ (mapcar (lambda (lst) (score (first lst) (gethash (second lst) *translation*))) *inputs*))))

(defun part-2 ()
  (let ((*inputs* (read-lists-of-symbols "02")))
    (flet ((translate (p1 p2)
             (ecase p2
               (y p1)
               (x (cond ((eq p1 +rock+) +scissors+)
                        ((eq p1 +paper+) +rock+)
                        ((eq p1 +scissors+) +paper+)))
               (z (cond ((eq p1 +rock+) +paper+)
                        ((eq p1 +paper+) +scissors+)
                        ((eq p1 +scissors+) +rock+))))))
      (reduce #'+ (mapcar (lambda (lst) (score (first lst) (translate (first lst) (second lst)))) *inputs*)))))
