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
  - \"--dynamic-space-size\" CLI argument can help with that."
  (loop with fish = (lanternfish)
        with zero = (loop for f in fish when (= 0 f) sum 1)
        with one = (loop for f in fish when (= 1 f) sum 1)
        with two = (loop for f in fish when (= 2 f) sum 1)
        with three = (loop for f in fish when (= 3 f) sum 1)
        with four = (loop for f in fish when (= 4 f) sum 1)
        with five = (loop for f in fish when (= 5 f) sum 1)
        with six = (loop for f in fish when (= 6 f) sum 1)
        with seven = (loop for f in fish when (= 7 f) sum 1)
        with eight = (loop for f in fish when (= 8 f) sum 1)
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
        do (list one two three four five six seven eight)
        finally (return (+ zero one two three four five six seven eight))))
