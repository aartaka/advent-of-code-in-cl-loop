(in-package :aoc-in-loop)

(defun folding ()
  (loop with fold-instructions = nil
        with raw-lines = (uiop:read-file-lines
                          (asdf:system-relative-pathname :aoc-in-loop "input/d13.in"))
        with dots = (loop for raw-line in raw-lines
                          until (uiop:emptyp raw-line)
                          collect (loopcar #'parse-integer
                                           (uiop:split-string raw-line :separator '(#\,))))
        with instructions = (loop with instructions-p = nil
                                  for raw-line in raw-lines
                                  for (raw-axis raw-number)
                                    = (when instructions-p
                                        (uiop:split-string
                                         (third (uiop:split-string raw-line))
                                         :separator '(#\=)))
                                  when instructions-p
                                    collect (if (string= raw-axis "x")
                                                (list (parse-integer raw-number) 0)
                                                (list 0 (parse-integer raw-number)))
                                  when (uiop:emptyp raw-line)
                                    do (setf instructions-p t))
        initially (return (list dots instructions))))

(defun fold (paper fold)
  (loop with (x-fold y-fold) = fold
        for (x-dot y-dot) in paper
        if (and (zerop x-fold)
                (> y-dot y-fold))
          collect (list x-dot (- y-fold (- y-dot y-fold)))
        else if (and (zerop y-fold)
                     (> x-dot x-fold))
               collect (list (- x-fold (- x-dot x-fold)) y-dot)
        else collect (list x-dot y-dot)))

(export 'd13-1)
(defun d13-1 ()
  (loop with (dots instructions) = (folding)
        initially (return (loopmove-duplicates (fold dots (first instructions))))))

(defun print-paper (paper)
  (loop with (max-x max-y) = (loop for (x y) in paper
                                   maximize x into max-x
                                   maximize y into max-y
                                   finally (return (list max-x max-y)))
        with strings = (loop for y upto max-y
                             collect (make-string (1+ max-x) :initial-element #\ ))
        for (x y) in paper
        do (setf (elt (elt strings y) x)
                 #\#)
        finally (print (loop with result = ""
                              for string in strings
                              do (setf result (concatenate 'string result (string #\Newline) string))
                              finally (return result)))))

(export 'd13-2)
(defun d13-2 ()
  (loop with (dots instructions) = (folding)
        with fold = dots
        for instruction in instructions
        do (setf fold (loopmove-duplicates (fold fold instruction)))
        finally (print-paper fold)))
