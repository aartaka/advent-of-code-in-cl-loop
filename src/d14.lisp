(in-package :aoc-in-loop)

(defun sequences ()
  (loop with file = (asdf:system-relative-pathname :aoc-in-loop "input/d14.in")
        with template = (coerce (uiop:read-file-line file) 'list)
        with raw-rules = (subseq (uiop:read-file-lines file) 2)
        with rules = (make-hash-table :test 'equalp)
        for raw-rule in raw-rules
        do (setf (gethash (coerce (subseq raw-rule 0 2) 'list) rules)
                 (elt raw-rule (1- (length raw-rule))))
        finally (return (list template rules))))

(defun incfhash (key table &optional (amount 1))
  (if (gethash key table)
      (incf (gethash key table) amount)
      (setf (gethash key table) amount)))

(defun pair-insert (&optional (steps 10))
  (loop with sequence-data = (sequences)
        with freqs = (make-hash-table :test 'equalp)
        with sequence = (first sequence-data)
        with rules = (second sequence-data)
        initially (loop for (a b . rest) on sequence
                        for pair = (list a b)
                        until (null b)
                        do (incfhash pair freqs))
        for step from 0 below steps
        do (setf freqs
                 (loop with new-freqs = (make-hash-table :test 'equalp)
                       for pair being the hash-key of rules
                         using (hash-value new)
                       for first-pair = (list (first pair) new)
                       for second-pair = (list new (second pair))
                       when (gethash pair freqs)
                         do (incfhash first-pair new-freqs
                                      (gethash pair freqs))
                         and do (incfhash second-pair new-freqs
                                          (gethash pair freqs))
                       finally (return new-freqs)))
        finally (return freqs)))

(defun d14 (&optional (steps 10))
  "TIL that:
- Lanternfish always hits you when you don't expect it.
- You can destructure the variables in the hash iteration `loop'.
- The pattern of increasing the hash value is appearing so often in
  AoC that I'm surprized no one has added such a function to any
  utility library that I'm aware of.
- Doing the problem using `reduce', `mapcar' &c. is almost twice as
  short as `loop' solution, especially with `alexandria:curry' and
  other functional idioms.
  - Does this mean I'm giving up on doing AoC in loop? Hell no!"
  (loop with result = (pair-insert steps)
        with freqs = (make-hash-table)
        for (first second) being the hash-key of result
          using (hash-value freq)
        do (incfhash first freqs freq)
        do (incfhash second freqs freq)
        finally (return (loop for freq being the hash-value of freqs
                              maximize freq into max
                              minimize freq into min
                              finally (return (- (ceiling (/ max 2)) (ceiling (/ min 2))))))))
