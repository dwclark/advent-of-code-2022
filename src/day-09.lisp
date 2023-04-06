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

(defun fix-up (prev rope)
  (if (not (touching-p (first rope) (second rope)))
      (let ((new-prev (second rope))
            (next-rope (cdr rope)))
        (setf (second rope) prev)
        (if (cdr next-rope)
            (fix-up new-prev next-rope)
            (add-visited (second rope))))))

(defun make-move (move rope)
  (dotimes (c (second move))
    (let ((prev (first rope)))
      (setf (first rope) (cons+ (first rope) (ecase (first move)
                                               (r '(0 . 1))
                                               (l '(0 . -1))
                                               (u '(1 . 0))
                                               (d '(-1 . 0)))))
      (fix-up prev rope))))
 
(defun play-game (n)
  (let ((*step* 0)
        (*moves* (read-lists-of-symbols "09"))
        (*visited* (make-hash-table :test #'equal))
        (rope (loop for i from 0 below n collecting (cons 0 0))))
    (add-visited (last rope))
    (dolist (move *moves*)
      (make-move move rope)
      (format t "~A. ~A~%" (incf *step*) rope))
    (hash-table-count *visited*)))

(defun part-1 ()
  (play-game 2))

(defun part-2 ()
  (play-game 10))

