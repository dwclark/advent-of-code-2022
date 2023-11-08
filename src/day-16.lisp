(defpackage :day-16
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :do-register-groups)
  (:import-from :alexandria :rcurry :curry :hash-table-alist)
  (:import-from :metabang.cl-containers :priority-queue-on-container :set-container :enqueue :dequeue :find-item :insert-new-item :empty-p)
  (:import-from :fare-memoization :define-memo-function)
  (:export #:part-1 #:part-2))

(declaim (optimize (debug 3)))

(in-package :day-16)

(defstruct node valve rate leads-to)

(defparameter *max-minutes* 0)
(defvar *nodes* nil)
(defvar *valves->nodes* nil)
(defvar *all-paths* nil)

(defun file->nodes ()
  (flet ((line->room (line)
           (do-register-groups ((#'intern src) (#'parse-integer rate) target) ("^Valve ([A-Z]+) has flow rate=(\\d+); tunnels? leads? to valves? (.+)$" line)
             (return (make-node :valve src :rate rate
                                :leads-to (read-from-string (concatenate 'string "(" (remove #\, target) ")")))))))
    (mapcar #'line->room (read-day-file "16"))))

(defun make-valves->nodes (nodes)
  (loop with ret = (make-hash-table :test #'eq)
        for node in nodes
        do (setf (gethash (node-valve node) ret) node)
        finally (return ret)))
    
(defun part-1 ()
  
  )

