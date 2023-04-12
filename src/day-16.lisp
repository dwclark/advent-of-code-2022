(defpackage :day-16
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :cl-ppcre :do-register-groups)
  (:import-from :alexandria :rcurry :curry :hash-table-alist)
  (:import-from :metabang.cl-containers :priority-queue-on-container :set-container :enqueue :dequeue :find-item :insert-new-item :empty-p)
  (:export #:part-1 #:part-2))

(declaim (optimize (debug 3)))

(in-package :day-16)

(defstruct tunnel valve rate leads-to)

(defparameter *max-minutes* 0)
(defvar *tunnels* nil)
(defvar *valves->tunnels* nil)
(defvar *max-flow* 0)
(defvar *all-paths* nil)
(defvar *valve-count* 0)

(defun compute-full-pressure (visited)
  (flet ((compute (c)
           (* (- *max-minutes* (second c)) (tunnel-rate (gethash (first c) *valves->tunnels*)))))
    (reduce #'+ (mapcar #'compute visited))))

(defun line->tunnel (line)
  (do-register-groups ((#'intern src) (#'parse-integer rate) target) ("^Valve ([A-Z]+) has flow rate=(\\d+); tunnels? leads? to valves? (.+)$" line)
    (return (make-tunnel :valve src :rate rate
                       :leads-to (read-from-string (concatenate 'string "(" (remove #\, target) ")"))))))
(defun file->tunnels ()
  (mapcar #'line->tunnel (read-day-file "16")))

(defun tunnels->hash-table ()
  (let ((ret (make-hash-table :test 'eq)))
    (dolist (tunnel *tunnels*)
      (setf (gethash (tunnel-valve tunnel) ret) tunnel))
    ret))

(defun compute-paths (src)
  (let ((pq (make-instance 'priority-queue-on-container :key #'cdr :sorter #'<))
        (visited (make-instance 'set-container))
        (paths (make-hash-table :test #'eq)))
    (enqueue pq (cons src 0))
    (loop while (not (empty-p pq))
          do (let* ((next (dequeue pq))
                    (current (car next))
                    (distance (cdr next))
                    (tunnel (gethash current *valves->tunnels*)))
               (insert-new-item visited current)
               (if (not (zerop (tunnel-rate tunnel)))
                   (setf (gethash current paths) distance))
               (loop for valve in (tunnel-leads-to tunnel)
                     do (if (not (find-item visited valve))
                            (enqueue pq (cons valve (1+ distance))))))
          finally (return paths))))

(defun compute-all-paths ()
  (loop with all-paths = (make-hash-table :test #'eq)
        for tunnel in *tunnels*
        do (let ((valve (tunnel-valve tunnel)))
             (setf (gethash valve all-paths) (compute-paths valve)))
        finally (return all-paths)))

(defun set-max-flow (visited)
  (setf *max-flow* (max *max-flow* (compute-full-pressure visited))))

(defun compute-scores (visited)
  (let* ((front (first visited))
         (valve (if (null front) 'aa (first front)))
         (minutes (if (null front) 1 (second front)))
         (open-minutes (if (null front) 0 1)))
    (when (or (<= *max-minutes* minutes)
              (= (length visited) *valve-count*))
      (set-max-flow visited)
      (return-from compute-scores))
    
    (loop for next-valve being the hash-keys in (gethash valve *all-paths*) using (hash-value cost-minutes)
          do (when (not (member next-valve visited :key #'car :test #'eq))
               (compute-scores (cons (list next-valve (+ open-minutes minutes cost-minutes)) visited))))))

(defun part-1 ()
  (let* ((*tunnels* (file->tunnels))
         (*valves->tunnels* (tunnels->hash-table))
         (*all-paths* (compute-all-paths))
         (*valve-count* (count-if #'(lambda (tunnel) (not (zerop (tunnel-rate tunnel)))) *tunnels*))
         (*max-flow* 0)
         (*max-minutes* 30))

    ;;(compute-scores 'aa nil 1)
    (compute-scores nil)
    *max-flow*))

(defun part-2 ()
  (let* ((*tunnels* (file->tunnels))
         (*valves->tunnels* (tunnels->hash-table))
         (*all-paths* (compute-all-paths))
         (*valve-count* (count-if #'(lambda (tunnel) (not (zerop (tunnel-rate tunnel)))) *tunnels*))
         (*max-flow* 0)
         (*max-minutes* 26))

    (compute-scores nil)
    *max-flow*))

