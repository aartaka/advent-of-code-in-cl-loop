(in-package :asdf)

(defsystem :aoc-in-loop
  :author "Artyom Bologov"
  :description "Solving Advent of Code 2021 using only CL loop macro."
  :license "BSD 2-Clause"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "generic")
               (:file "d1")
               (:file "d2")
               (:file "d3")
               (:file "d4")
               (:file "d5")
               (:file "d6")
               (:file "d7")
               (:file "d8")
               (:file "d9")
               (:file "d10")))
