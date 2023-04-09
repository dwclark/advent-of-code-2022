(defpackage :day-13
  (:use :cl)
  (:import-from :utils :read-day-file)
  (:import-from :split-sequence :split-sequence)
  (:export #:part-1 #:part-2))

(declaim (optimize (debug 3)))

(in-package :day-13)

(defun load-pairs ()
  (let ((grouped-strs (split-sequence "" (read-day-file "13") :test #'string=)))
    (mapcar (lambda (lst)
              (mapcar #'read-from-string lst)) grouped-strs)))

(defun list< (left right)
  (loop with left-size = (length left)
        with right-size = (length right)
        with result = :continue
        for left-item in left
        for right-item in right
        do (progn
             (setf result (cond ((and (atom left-item) (not (listp left-item))
                                      (atom right-item) (not (listp right-item)))
                                 (cond ((< left-item right-item)
                                        :true)
                                       ((> left-item right-item)
                                        :false)
                                       
                                       (t :continue)))
                                
                                ((and (listp left-item) (listp right-item))
                                 (list< left-item right-item))
                                
                                ((and (listp left-item) (atom right-item))
                                 (list< left-item (list right-item)))
                                
                                ((and (atom left-item) (listp right-item))
                                 (list< (list left-item) right-item))

                                (t (error "should no be here"))))

             (if (or (eq :true result) (eq :false result))
                 (return result)))
        finally (return (cond ((< left-size right-size)
                               :true)
                              
                              ((> left-size right-size)
                               :false)
                              
                              (t :continue)))))
               

(defun part-1 ()
  (flet ((start (pair) (list< (first pair) (second pair))))
    (loop with results = (mapcar #'start (load-pairs))
          with ret = nil
          for result in results
          for idx from 0 below (length results)
          do (if (eq :true result)
                 (push (1+ idx) ret))
          finally (return (reduce #'+ ret)))))

(defun part-2 ()
  (let* ((pairs (load-pairs))
         (all (loop with ret = (list '((2)) '((6)))
                    for pair in pairs
                    do (progn
                         (push (first pair) ret)
                         (push (second pair) ret))
                    finally (return ret)))
         (sorted (sort all #'(lambda (f s)
                               (if (eq :true (list< f s)) t nil)))))

    (* (1+ (position '((2)) sorted :test #'equal))
       (1+ (position '((6)) sorted :test #'equal)))))
         
                 
    
        
