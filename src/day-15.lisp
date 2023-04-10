(defpackage :day-15
  (:use :cl)
  (:import-from :utils :read-day-file :range-overlap-p)
  (:import-from :cl-ppcre :do-register-groups)
  (:import-from :alexandria :rcurry :curry)
  (:export #:part-1 #:part-2))

(declaim (optimize (debug 3)))

(in-package :day-15)

(defstruct sb sensor-x sensor-y beacon-x beacon-y)

(defun manhattan (pair)
  (+ (abs (- (sb-sensor-x pair) (sb-beacon-x pair)))
     (abs (- (sb-sensor-y pair) (sb-beacon-y pair)))))

(defun y-range (pair)
  (let ((dist (manhattan pair))
        (y (sb-sensor-y pair)))
    (cons (- y dist) (+ y dist))))

(defun intersects-y-p (pair y-val)
  (destructuring-bind (low-y . high-y) (y-range pair)
    (<= low-y y-val high-y)))

(defun x-range-at-y (pair y)
  (let* ((dist (manhattan pair))
         (dist-to-y (abs (- y (sb-sensor-y pair))))
         (diff (- dist dist-to-y)))
    (cons (- (sb-sensor-x pair) diff) (+ (sb-sensor-x pair) diff))))

(defun beacons-at-y (pairs y)
  (remove-if-not (curry #'= y) pairs :key #'sb-beacon-y))
                       
(defun line->sb-pair (line)
  (do-register-groups ((#'parse-integer sx sy bx by)) ("^Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)$" line)
    (return (make-sb :sensor-x sx :sensor-y sy :beacon-x bx :beacon-y by))))

(defun parse-pairs ()
  (mapcar #'line->sb-pair (read-day-file "15")))

(defun coalesce-ranges (ranges)
  (loop with sorted = (sort ranges #'< :key #'car)
        with ret = nil
        while sorted
        do (loop with src = (pop sorted)
                 do (let ((next (first sorted)))
                      (if (and next (range-overlap-p src next))
                          (progn
                            (setf src (cons (car src)
                                            (max (cdr src) (cdr next))))
                            (pop sorted))
                          (progn
                            (push src ret)
                            (return)))))
        finally (return (reverse ret))))

(defun part-1 ()
  (let* ((y-at 2000000)
         (pairs (parse-pairs))
         (intersecting (remove-if-not (rcurry #'intersects-y-p y-at) pairs))
         (x-ranges (mapcar (rcurry #'x-range-at-y y-at) intersecting))
         (beacons-x-vals (remove-duplicates (mapcar #'sb-beacon-y (beacons-at-y pairs y-at))))
         (min-x (apply #'min (mapcar #'car x-ranges)))
         (max-x (apply #'max (mapcar #'cdr x-ranges))))
    (loop with counter = 0
          for x from min-x to max-x
          do (if (and (find-if #'(lambda (range) (<= (car range) x (cdr range))) x-ranges)
                      (not (member x beacons-x-vals)))
                 (incf counter))
          finally (return counter))))

(defun part-2 ()
  (loop with pairs = (parse-pairs)
        with beacons-at = (mapcar #'(lambda (pair) (cons (sb-beacon-x pair) (sb-beacon-y pair))) pairs)
        with min-val = 0
        with max-val = 4000000
        for y from min-val to max-val
        do (let* ((intersecting (remove-if-not (rcurry #'intersects-y-p y) pairs))
                  (x-ranges (mapcar (rcurry #'x-range-at-y y) intersecting))
                  (coalesced (coalesce-ranges x-ranges)))
             (if (= 2 (length coalesced))
                 (let ((x (1+ (cdr (first coalesced)))))
                   (if (and (<= min-val x max-val)
                            (not (member (cons x y) beacons-at :test #'equal)))
                       (return-from part-2 (+ (* max-val x) y))))))))
