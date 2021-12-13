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

(defun d13-1 ()
  (loop with (dots instructions) = (folding)
        initially (return (loopmove-duplicates (fold dots (first instructions))))))
