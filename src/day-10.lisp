(defpackage :day-10
  (:use :cl)
  (:import-from :utils :read-lists-of-symbols)
  (:import-from :alexandria :curry :plist-hash-table)
  (:export #:part-1 #:part-2))

(in-package :day-10)

(defparameter *of-interest* (plist-hash-table '(20 t 60 t 100 t 140 t 180 t 220 t)))

(defvar *code* nil)
(defvar *register* 1)
(defvar *accum* 0)

(defvar *ip* 0)
(defvar *cycles* 1)
(defvar *ip-ticks* 0)

(defvar *screen* nil)

(defun initial-screen ()
  (make-array 6 :initial-contents (list (make-array 40 :element-type 'character :initial-element #\.)
                                        (make-array 40 :element-type 'character :initial-element #\.)
                                        (make-array 40 :element-type 'character :initial-element #\.)
                                        (make-array 40 :element-type 'character :initial-element #\.)
                                        (make-array 40 :element-type 'character :initial-element #\.)
                                        (make-array 40 :element-type 'character :initial-element #\.))))

(defun draw-pixel ()
  (let* ((crt-row (floor (/ (1- *cycles*) 40)))
         (crt-col (mod (1- *cycles*) 40))
         (sprite-position *register*)
         (sprite-low (1- sprite-position))
         (sprite-high (1+ sprite-position)))
    
    (if (<= sprite-low crt-col sprite-high) 
        (setf (aref (aref *screen* crt-row) crt-col) #\#))))
  
(defun cycle-clock ()
  (draw-pixel)
  
  (if (gethash *cycles* *of-interest*)
      (let ((augment (* *cycles* *register*)))
        (incf *accum* augment)))

  (incf *cycles*)
  (incf *ip-ticks*)
  (let* ((segment (aref *code* *ip*))
         (instruction (first segment))
         (to-add (second segment)))
    (ecase instruction
      (noop (progn
              (setf *ip-ticks* 0)
              (incf *ip*)))
      (addx (if (= 2 *ip-ticks*)
                (progn
                  (incf *register* to-add)
                  (setf *ip-ticks* 0)
                  (incf *ip*))))))

  (< *ip* (length *code*)))

(defun run-code ()
  (let ((*code* (map 'vector #'identity (read-lists-of-symbols "10")))
        (*screen* (initial-screen))
        (*register* 1)
        (*accum* 0)
        (*ip* 0)
        (*cycles* 1)
        (*ip-ticks* 0))
    (loop while (cycle-clock))
    (values *accum* *screen*)))

(defun part-1 ()
  (first (multiple-value-list (run-code))))

(defun part-2 ()
  (second (multiple-value-list (run-code))))
