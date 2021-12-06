(in-package :aoc-in-loop)

(defun lines ()
  (loop with input-lines = (uiop:read-file-lines (asdf:system-relative-pathname :aoc-in-loop "input/d5.in"))
        with raw-lines = (loopcar #'uiop:split-string input-lines)
        with lines = (loop for raw-line in raw-lines
                           collect (list (loopcar #'parse-integer
                                                  (uiop:split-string (first raw-line)
                                                                     :separator '(#\,)))
                                         (loopcar #'parse-integer
                                                  (uiop:split-string (third raw-line)
                                                                     :separator '(#\,)))))
        initially (return lines)))

(defun horisontal-p (line)
  (= (first (first line))
     (first (second line))))

(defun vertical-p (line)
  (= (second (first line))
     (second (second line))))

(defun direction (number)
  (cond
    ((plusp number) 1)
    ((zerop number) 0)
    ((minusp number) -1)))

(export 'd5-2)
(defun d5-2 ()
  "TIL that:
- CL:LOOP has no generic arithmetic stepping (e.g., to work both from -5 to 5 and from 5 to -5).
  - To have this generic stepping, a form like `FOR X = X0 THEN (INCF X BY)' would work,
    but this is essentially a C-like loop :/
- Off-by-one errors are indeed dreadful.

How do I solve this without a two-dimentional array?"
  (loop with lines = (lines)
        with x-dimension = (1+ (loop for ((x1 __) (x2 _)) in lines
                                     maximize (max x1 x2)))
        with y-dimension = (1+ (loop for ((x1 y1) (x2 y2)) in lines
                                     maximize (max y1 y2)))
        with map = (make-array (list x-dimension y-dimension) :initial-element 0)
        with fill-map
          = (loop for ((x1 y1) (x2 y2)) in lines
                  do (loop for x = x1 then (incf x (direction (- x2 x1)))
                           for y = y1 then (incf y (direction (- y2 y1)))
                           until (and (= x x2) (= y y2))
                           do (incf (aref map x y))
                           finally (incf (aref map x y))))
        for x from 0 below (array-dimension map 0)
        sum (loop for y from 0 below (array-dimension map 1)
                  count (> (aref map x y) 1))))
