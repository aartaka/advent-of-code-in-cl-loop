(in-package :aoc-in-loop)

(defun syntax-lines ()
  (uiop:read-file-lines
   (asdf:system-relative-pathname :aoc-in-loop "input/d10.in")))

(defun assoc-value (key list)
  (loop for (k v) in list
        when (equal key k)
          do (return v)))

(defun check-lines (&optional (lines (syntax-lines)))
  (loop for line in lines
        collect (loop with matching-brackets
                        = '((#\) #\()
                            (#\] #\[)
                            (#\} #\{)
                            (#\> #\<))
                      with stack = (list)
                      for char across line
                      for column from 0 upto (length line)
                      if (loopind char '(#\( #\[ #\{ #\<))
                        do (push char stack)
                      else if (loopind char '(#\) #\] #\} #\>))
                             do (if (equal (assoc-value char matching-brackets)
                                           (first stack))
                                    (pop stack)
                                    (return (list :corrupted column char)))
                      finally (return
                                (if stack
                                    (list :incomplete stack)
                                    (list :complete))))))

(defun d10-1 ()
  (loop with score-table = '((#\) 3)
                             (#\] 57)
                             (#\} 1197)
                             (#\> 25137))
        with lines = (syntax-lines)
        with check-stata = (check-lines lines)
        for (status . args) in check-stata
        when (eq :corrupted status)
          sum (assoc-value (second args) score-table)))

(defun d10-2 ()
  "TIL that:
- Lisp is (unsurprisingly) good for parsers.
- I don't remember whether alists consist of (KEY . VALUE) or (KEY
  VALUE) entries.
  - I've gone with the second one, although the first one is most
    probably the correct way to do alists.
- Stacks are a cool data structure, and knowing about stacks saved me
  today. Especially if it's such a thing as paren-stack."
  (loop with score-table = '((#\( 1)
                             (#\[ 2)
                             (#\{ 3)
                             (#\< 4))
        with lines = (syntax-lines)
        with check-stata = (check-lines lines)
        for (status . args) in check-stata
        when (eq :incomplete status)
          collect (loop with score = 0
                        with stack = (first args)
                        while stack
                        do (setf score
                                 (+ (* 5 score)
                                    (assoc-value (pop stack) score-table)))
                        finally (return score))
            into scores
        finally (return (elt (loop-sort scores) (floor (/ (length scores) 2))))))
