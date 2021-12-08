(in-package :aoc-in-loop)

(defun letters->integers (string)
  (loop for letter across string
        collect (- (char-code letter) (char-code #\a))))

(defun segment-digits ()
  (loop for line in (uiop:read-file-lines
                     (asdf:system-relative-pathname :aoc-in-loop "input/d8.in"))
        for split-line = (uiop:split-string line :separator '(#\|))
        collect (list (loopcar #'letters->integers
                               (loopmove-if #'uiop:emptyp
                                            (uiop:split-string (first split-line))))
                      (loopcar #'letters->integers
                               (loopmove-if #'uiop:emptyp
                                            (uiop:split-string (second split-line)))))))

(defun d8-1 ()
  (loop for (in out) in (segment-digits)
        sum (loop for o in out
                  count (= (length o) 2)
                  count (= (length o) 3)
                  count (= (length o) 4)
                  count (= (length o) 7))))

(defun find-by-length (list length)
  (loop for e in list
        when (= (length e) length)
          do (return e)))

(defun orderless-equal (a b)
  (zerop (+ (length (loop-diff a b))
            (length (loop-diff b a)))))

(defun decode-segments (inputs)
  (loop with one = (find-by-length inputs 2)
        with three = (loop for in in inputs
                           when (and (= (length in) 5)
                                     (orderless-equal in (loop-unite one in)))
                             do (return in))
        with seven = (find-by-length inputs 3)
        with six = (loop for in in inputs
                         when (and (= (length in) 6)
                                   (= (length (loop-intersect in one)) 1))
                           do (return in))
        with four = (find-by-length inputs 4)
        with eight = (find-by-length inputs 7)
        with nine = (loop for in in inputs
                          when (and (= (length in) 6)
                                    (= (length (loop-diff in three)) 1))
                            do (return in))
        with zero = (loop for in in inputs
                          when (and (= (length in) 6)
                                    (not (orderless-equal in nine))
                                    (not (orderless-equal in six)))
                            do (return in))
        with five = (loop for in in inputs
                          when (and (= (length in) 5)
                                    (= (length (loop-intersect in six)) 5))
                            do (return in))
        with two = (loop for in in inputs
                         when (and (= (length in) 5)
                                   (not (orderless-equal in five))
                                   (not (orderless-equal in three)))
                           do (return in))
        initially (return (list zero one two three four five six seven eight nine))))

(defun d8-2 ()
  "TIL that:
- `loop' does get cumbersome on the tasks it's not really well-tailored for.
  - One, however, can never encounter such tasks.
- `loop' is bad for logic programming, as building sets of conditions
  for it is too wordy, compared to e.g., `cond' or `case'.
- Strings are better transformed into lists, as loop is still bad with arrays.
- Loop is bad at type-checking. Well... any compiler is bad at type
  checking, because it doesn't know what my domain is. If my domain is
  sets of numbers that always have the number I look for, I can afford
  having `when x do (return y)' with no `finally (return z)' simply
  because my domain allows that. Compilers have no knowledge of this,
  unfortunately."
  (loop for (inputs outputs) in (segment-digits)
        for decoding = (decode-segments inputs)
        sum (loop for order from 3 downto 0
                  for out in outputs
                  sum (* (expt 10 order)
                         (loop for de in decoding
                               for i upto 9
                               when (orderless-equal de out)
                                 do (return i))))))
