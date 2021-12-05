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

(defun d5-1 ()
  (loop with lines = (lines)
        with x-dimension = (1+ (loop for ((x1 y1) (x2 y2)) in (aoc::lines)
                                     maximize (max x1 x2)))
        with y-dimension = (1+ (loop for ((x1 y1) (x2 y2)) in (aoc::lines)
                                     maximize (max y1 y2)))
        with map = (make-array (list x-dimension y-dimension) :initial-element 0)
        with tmp
          = (loop for ((x1 y1) (x2 y2)) in lines
                  for line in lines
                  do (loop with xs = (loop for x from (min x1 x2) to (max x2 x1)
                                           collect x into xs
                                           finally (return (if (> x1 x2) (loopverse xs) xs)))
                           with ys = (loop for y from (min y1 y2) to (max y2 y1)
                                           collect y into ys
                                           finally (return (if (> y1 y2) (loopverse ys) ys)))
                           for x in (if (= (length xs) 1)
                                        (make-list (length ys) :initial-element (first xs))
                                        xs)
                           for y in (if (= (length ys) 1)
                                        (make-list (length xs) :initial-element (first ys))
                                        ys)
                           do (incf (aref map x y))))
        for x from 0 below (array-dimension map 0)
        sum (loop for y from 0 below (array-dimension map 1)
                  count (> (aref map x y) 1))))
