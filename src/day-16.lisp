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

(defstruct tunnel valve rate leads-to)

(defparameter *max-minutes* 0)
(defvar *tunnels* nil)
(defvar *valves->tunnels* nil)
(defvar *all-paths* nil)

(defun compute-pressure (valve minutes)
  (* (tunnel-rate (gethash valve *valves->tunnels*)) (- *max-minutes* (1- minutes))))

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
                   (setf (gethash current paths) (1+ distance))) ;account turn time in weighting
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

(defun compute-scores (valve minutes remaining)
  (flet ((compute-next (next)
           (let* ((current-paths (gethash valve *all-paths*))
                  (to-add (gethash next current-paths)))
             (+ (compute-pressure valve minutes) (compute-scores next (+ to-add minutes) (remove next remaining))))))
    
    (cond ((<= *max-minutes* minutes) 0) ;ran out of time
          ((null remaining) (compute-pressure valve minutes)) ;last one to open
          (t (apply #'max (mapcar #'compute-next remaining))))))

(defun compute-scores-2 (mover m-valve m-minutes e-valve e-minutes remaining)
  (let ((valve (if (eq :man mover) m-valve e-valve))
        (minutes (if (eq :man mover) m-minutes e-minutes)))
    (flet ((compute-next (next)
             (let* ((next-mover (if (<= m-minutes e-minutes) :man :elephant))
                    (mover-paths (gethash (if (eq :man next-mover) m-valve e-valve) *all-paths*))
                    (to-add (gethash next mover-paths)))
               (+ (compute-pressure valve minutes)
                  (if (eq :man next-mover)
                      (compute-scores-2 next-mover next (+ m-minutes to-add) e-valve e-minutes (remove next remaining))
                      (compute-scores-2 next-mover m-valve m-minutes next (+ e-minutes to-add) (remove next remaining)))))))
                  
    
      (cond ((<= *max-minutes* minutes) 0) ;ran out of time
            ((null remaining) (compute-pressure valve minutes)) ;last one to open
            (t (apply #'max (mapcar #'compute-next remaining)))))))

(defun play-game (time func)
  (let* ((*tunnels* (file->tunnels))
         (*valves->tunnels* (tunnels->hash-table))
         (*all-paths* (compute-all-paths))
         (remaining (mapcar #'tunnel-valve (remove-if #'(lambda (tunnel) (zerop (tunnel-rate tunnel))) *tunnels*)))
         (*max-minutes* time))
    (funcall func remaining)))

(defun part-1 ()
  (play-game 30 (curry #'compute-scores 'aa 1)))

(defun part-2 ()
  (play-game 26 (curry #'compute-scores-2 :man 'aa 1 'aa 1)))
