(defpackage :day-11
  (:use :cl)
  (:import-from :utils :read-lists-of-symbols)
  (:import-from :split-sequence :split-sequence)
  (:import-from :alexandria :curry :plist-hash-table)
  (:export #:part-1 #:part-2))

(in-package :day-11)

(defstruct monkey id items expr divisible-by true-throw false-throw inspected-items)

(defvar *monkeys* nil)
(defvar *residues* nil)
(defvar old nil)

(defun monkey-operation (monkey prev)
  (let ((old prev))
    (eval (monkey-expr monkey))))

(defun monkey-stage (monkey reduce-worry)
  (let ((items (monkey-items monkey)))
    (setf (monkey-items monkey) nil)
    (loop for item in items
          do (let* ((op-result (funcall reduce-worry (monkey-operation monkey item)))
                    (throw-to-num (if (zerop (mod op-result (monkey-divisible-by monkey)))
                                      (monkey-true-throw monkey)
                                      (monkey-false-throw monkey)))
                    (throw-to-monkey (nth throw-to-num *monkeys*)))
               (incf (monkey-inspected-items monkey))
               (setf (monkey-items throw-to-monkey) (append (monkey-items throw-to-monkey) (list op-result)))))))
        

(defun make-monkeys (list-symbols)
  (loop for monkey-symbols in list-symbols
        collect (let ((id-clause (first monkey-symbols))
                      (starting-clause (second monkey-symbols))
                      (op-clause (third monkey-symbols))
                      (divisible-by-clause (fourth monkey-symbols))
                      (true-clause (fifth monkey-symbols))
                      (false-clause (sixth monkey-symbols)))
                  (make-monkey :id (second id-clause) :items (cddr starting-clause) :inspected-items 0
                               :expr (list (fifth op-clause) (fourth op-clause) (sixth op-clause))
                               :divisible-by (car (last divisible-by-clause))
                               :true-throw (car (last true-clause))
                               :false-throw (car (last false-clause)))) into monkeys
        finally (return monkeys)))

(defun populate-monkeys ()
  (let ((list-symbols (split-sequence nil (read-lists-of-symbols "11")
                                      :test #'(lambda (ignore o) (declare (ignore ignore)) (null o)))))
    (make-monkeys list-symbols)))
(defun play-game (times reduce-worry)
  (let* ((list-symbols (split-sequence nil (read-lists-of-symbols "11")
                                       :test #'(lambda (ignore o) (declare (ignore ignore)) (null o))))
         (*monkeys* (make-monkeys list-symbols))
         (*residues* (reduce #'* (mapcar #'monkey-divisible-by *monkeys*))))
    (dotimes (n times)
      (dolist (monkey *monkeys*)
        (monkey-stage monkey reduce-worry)))
    
    (let ((sorted (sort (mapcar #'monkey-inspected-items *monkeys*) #'>)))
      (* (first sorted) (second sorted)))))


(defun part-1 ()
  (play-game 20 #'(lambda (val) (floor (/ val 3)))))

(defun part-2 ()
  (flet ((reduce-worry (val)
           (mod val *residues*)))
    (play-game 10000 #'reduce-worry)))
