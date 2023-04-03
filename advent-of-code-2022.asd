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
               (:file "src/day-05" :depends-on ("src/utils"))))
