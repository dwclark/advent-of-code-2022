(asdf:defsystem #:advent-of-code-2022
  :description "Advent of Code 2022"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :depends-on ("alexandria" "cl-ppcre" "fare-memoization" "array-operations" "infix-math" "cl-heap"
                            "cl-containers" "split-sequence")
  :components ((:file "src/utils")
               (:file "src/day-01" :depends-on ("src/utils"))
               (:file "src/day-02" :depends-on ("src/utils"))
               (:file "src/day-03" :depends-on ("src/utils"))
               (:file "src/day-04" :depends-on ("src/utils"))
               (:file "src/day-05" :depends-on ("src/utils"))
               (:file "src/day-06" :depends-on ("src/utils"))
               (:file "src/day-07" :depends-on ("src/utils"))
               (:file "src/day-08" :depends-on ("src/utils"))
               (:file "src/day-09" :depends-on ("src/utils"))
               (:file "src/day-10" :depends-on ("src/utils"))
               (:file "src/day-11" :depends-on ("src/utils"))
               (:file "src/day-12" :depends-on ("src/utils"))
               (:file "src/day-13" :depends-on ("src/utils"))
               (:file "src/day-14" :depends-on ("src/utils"))
               (:file "src/day-15" :depends-on ("src/utils"))
               (:file "src/day-16" :depends-on ("src/utils"))
	       (:file "src/day-18" :depends-on ("src/utils"))
	       (:file "src/day-19" :depends-on ("src/utils"))
	       (:file "src/day-20" :depends-on ("src/utils"))
	       (:file "src/day-21" :depends-on ("src/utils"))
	       (:file "src/day-22" :depends-on ("src/utils"))
	       (:file "src/day-23" :depends-on ("src/utils"))
	       (:file "src/day-24" :depends-on ("src/utils"))))
