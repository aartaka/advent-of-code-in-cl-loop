(in-package :aoc-in-loop)

(defun segment-digits ()
  (loop for line in (uiop:read-file-lines
                     (asdf:system-relative-pathname :aoc-in-loop "input/d8.in"))
        for split-line = (uiop:split-string line :separator '(#\|))
        collect (list (loopmove-if #'uiop:emptyp (uiop:split-string (first split-line)))
                      (loopmove-if #'uiop:emptyp (uiop:split-string (second split-line))))))

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

(defun decode-segments (inputs)
  (loop with one = (find-by-length inputs 2)
        with seven = (find-by-length inputs 3)
        with four = (find-by-length inputs 4)
        with eight = (find-by-length inputs 7)
        with six = ))
