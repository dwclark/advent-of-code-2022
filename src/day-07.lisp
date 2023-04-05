(defpackage :day-07
  (:use :cl)
  (:import-from :utils :read-lists-of-symbols)
  (:import-from :alexandria :rcurry)
  (:export #:part-1 #:part-2))

(in-package :day-07)

(defstruct file-entry name size)
(defstruct dir-entry name subs files size)

(defun process-dir (root vec index)
  (loop with current-index = index
        while (< current-index (length vec))
        do (let ((entry (aref vec current-index)))
             (incf current-index)
             (cond ((and (eq '$ (first entry)) (eq 'ls (second entry)))
                    nil)

                   ((eq 'dir (first entry))
                    (push (make-dir-entry :name (second entry)) (dir-entry-subs root)))
                   
                   ((numberp (first entry))
                    (push (make-file-entry :size (first entry) :name (second entry)) (dir-entry-files root)))
                   
                   ((and (eq '$ (first entry)) (eq 'cd (second entry)))
                    (let ((dir-name (third entry)))
                      (if (eq '^^ dir-name)
                          (return-from process-dir current-index)
                          (let ((next-root (find dir-name (dir-entry-subs root) :key #'dir-entry-name :test #'eq)))
                            (setf current-index (process-dir next-root vec current-index))))))))
        finally (return current-index)))

(defun compute-sizes (root)
  (setf (dir-entry-size root)
        (+ (reduce #'+ (dir-entry-files root) :key #'file-entry-size)
           (reduce #'+ (mapcar #'compute-sizes (dir-entry-subs root))))))

(defun flatten-dirs (root vec)
  (vector-push-extend root vec)
  (loop for entry in (dir-entry-subs root)
        do (flatten-dirs entry vec))
  vec)

(defun run-commands ()
  (let* ((cmds (map 'vector #'identity (read-lists-of-symbols "07")))
         (root (make-dir-entry :name (third (aref cmds 0)))))
    (process-dir root cmds 1)
    (compute-sizes root)
    root))

(defun part-1 ()
  (let ((root (run-commands)))
    (reduce #'+ (remove-if-not (rcurry #'<= 100000)
                               (map 'list #'dir-entry-size (flatten-dirs root (make-array 0 :adjustable t :fill-pointer 0)))))))

(defun part-2 ()
  (let* ((root (run-commands))
         (total-size (dir-entry-size root))
         (needed (- 30000000 (- 70000000 total-size)))
         (vec (flatten-dirs root (make-array 0 :adjustable t :fill-pointer 0))))
    (first (sort (remove-if (rcurry #'<= needed) (map 'list #'dir-entry-size vec)) #'<))))

