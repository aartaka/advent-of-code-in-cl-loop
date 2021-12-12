(in-package :aoc-in-loop)

(defun octopuses ()
  (read-matrix (asdf:system-relative-pathname :aoc-in-loop "input/d11.in")))

(defun 8-neighbors (point map)
  (loop for (xdiff ydiff) in '((0 1) (1 0) (0 -1) (-1 0) (1 1) (1 -1) (-1 1) (-1 -1))
        for x = (first point)
        for y = (second point)
        when (and (<= 0 (+ xdiff x) (1- (length map)))
                  (<= 0 (+ ydiff y) (1- (length (first map)))))
          collect (list (+ xdiff x) (+ ydiff y))))

(defun flash (map)
  (loop with flash-count = 0
        with flashed-already = '()
        with flashers = (loop for x below (length map)
                              append (loop for y below (length (first map))
                                           when (> (mref map x y) 9)
                                             collect (list x y)))
        while flashers
        do (loop with (x y) = (pop flashers)
                 initially (setf (mref map x y) 0)
                 initially (incf flash-count)
                 initially (pushnew (list x y) flashed-already :test #'equal)
                 for (nx ny) in (8-neighbors (list x y) map)
                 unless (loopind (list nx ny) flashed-already)
                   do (incf (mref map nx ny))
                   and when (> (mref map nx ny) 9)
                         do (pushnew (list nx ny) flashers :test #'equal))
        finally (return flash-count)))

(defun d11-1 (&optional (steps 100))
  (loop with map = (octopuses)
        for step below steps
        do (loop for x below (length map)
                 do (loop for y below (length (first map))
                          do (incf (mref map x y))))
        sum (flash map)))

(defun d11-2 ()
  (loop with map = (octopuses)
        for step from 0 by 1
        do (loop for x below (length map)
                 do (loop for y below (length (first map))
                          do (incf (mref map x y))))
        when (= (* (length map) (length (first map)))
                (flash map))
          do (return (1+ step))))
