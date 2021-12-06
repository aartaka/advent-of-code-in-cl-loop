(in-package :aoc-in-loop)

(defun lanternfish ()
  (loop for raw-fish in (uiop:split-string
                         (uiop:read-file-line
                          (asdf:system-relative-pathname :aoc-in-loop "input/d6.in"))
                         :separator '(#\,))
        collect (parse-integer raw-fish)))

(export 'd6)
(defun d6 (&optional (days 80))
  "TIL that
- Not all the data is worthy of storage, sometimes it's only the general info that you need.
  - As an extension: if you have lots of repeating items it's easier do split those into buckets.
- Sometimes `psetf' is inevitable.
- SBCL has a surprisingly small stack size, and it blows up on some two megabytes of data.
  - \"--dynamic-space-size\" CLI argument can help with that.
- when condition sum 1 == count condition"
  (loop with fish = (lanternfish)
        with zero = (loop for f in fish count (= 0 f))
        with one = (loop for f in fish count (= 1 f))
        with two = (loop for f in fish count (= 2 f))
        with three = (loop for f in fish count (= 3 f))
        with four = (loop for f in fish count (= 4 f))
        with five = (loop for f in fish count (= 5 f))
        with six = (loop for f in fish count (= 6 f))
        with seven = (loop for f in fish count (= 7 f))
        with eight = (loop for f in fish count (= 8 f))
        for day from 1 upto days
        do (psetf eight zero
                  seven eight
                  six (+ zero seven)
                  five six
                  four five
                  three four
                  two three
                  one two
                  zero one)
        finally (return (+ zero one two three four five six seven eight))))
