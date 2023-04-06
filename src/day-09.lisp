(defpackage :day-09
  (:use :cl)
  (:import-from :utils :read-lists-of-symbols :*to-add/8*)
  (:import-from :alexandria :curry)
  (:export #:part-1 #:part-2))

(in-package :day-09)

(defvar *step* 0)
(defvar *moves* nil)
(defvar *visited* nil)
(defvar *to-add* (concatenate 'list '((0 . 0)) *to-add/8*))

(defun cons+ (c1 c2)
  (cons (+ (car c1) (car c2)) (+ (cdr c1) (cdr c2))))

(defun touching-p (head tail)
  (find-if (curry #'equal head) *to-add* :key (curry #'cons+ tail)))

(defun add-visited (cell)
  (setf (gethash (copy-tree cell) *visited*) t))

(defun row= (c1 c2)
  (= (car c1) (car c2)))

(defun row< (c1 c2)
  (< (car c1) (car c2)))

(defun row> (c1 c2)
  (> (car c1) (car c2)))

(defun col= (c1 c2)
  (= (cdr c1) (cdr c2)))

(defun col< (c1 c2)
  (< (cdr c1) (cdr c2)))

(defun col> (c1 c2)
  (> (cdr c1) (cdr c2)))

(defun fix-up (rope)
  (let ((head (first rope))
        (tail (second rope)))
    (if (not (touching-p head tail))
        (let ((to-add (cond ((row= head tail)
                             (if (col< head tail) '(0 . -1) '(0 . 1)))
                            
                            ((col= head tail)
                             (if (row< head tail) '(-1 . 0) '(1 . 0)))
                            
                            ((and (row< head tail) (col< head tail)) '(-1 . -1))
                            
                            ((and (row< head tail) (col> head tail)) '(-1 . 1))
                            
                            ((and (row> head tail) (col< head tail)) '(1 . -1))
                            
                            ((and (row> head tail) (col> head tail)) '(1 . 1))
                            
                            (t (error "bad condition")))))
          (let ((next-rope (cdr rope))
                (new-val (setf (second rope) (cons+ tail to-add))))
            (if (cdr next-rope)
                (fix-up next-rope)
                (add-visited new-val)))))))

(defun make-move (move rope)
  (dotimes (c (second move))
    (setf (first rope) (cons+ (first rope) (ecase (first move)
                                             (r '(0 . 1))
                                             (l '(0 . -1))
                                             (u '(1 . 0))
                                             (d '(-1 . 0)))))
    (fix-up rope)))

(defun play-game (n)
  (let ((*step* 0)
        (*moves* (read-lists-of-symbols "09"))
        (*visited* (make-hash-table :test #'equal))
        (rope (loop for i from 0 below n collecting (cons 0 0))))
    (add-visited (last rope))
    (dolist (move *moves*)
      (make-move move rope))
    (hash-table-count *visited*)))

(defun part-1 ()
  (play-game 2))

(defun part-2 ()
  (play-game 10))

