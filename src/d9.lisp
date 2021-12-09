(in-package :aoc-in-loop)

(defun heightmap ()
  (loop for line in (uiop:read-file-lines
                     (asdf:system-relative-pathname :aoc-in-loop "input/d9.in"))
        collect (loop for char across line
                      collect (- (char-code char) (char-code #\0)))))

(defun mref (map x y)
  (elt (elt map x) y))

(defun neighbors (point map)
  (loop for (xdiff ydiff) in '((0 1) (1 0) (0 -1) (-1 0))
        for x = (first point)
        for y = (second point)
        when (and (<= 0 (+ xdiff x) (1- (length map)))
                  (<= 0 (+ ydiff y) (1- (length (first map)))))
          collect (list (+ xdiff x) (+ ydiff y))))

(defun low-points (map)
  (loop for x below (length map)
        append (loop for y below (length (first map))
                     when (loop for neigh in (neighbors (list x y) map)
                                unless (< (mref map x y)
                                          (mref map (first neigh) (second neigh)))
                                  do (return nil)
                                finally (return t))
                       collect (list x y))))

(defun d9-1 ()
  (loop with map = (heightmap)
        for (x y) in (low-points map)
        sum (1+ (mref map x y))))

(defun drain-frontier (frontier map)
  (loop with basin = nil
        while frontier
        do (setf frontier
                 (loop for f in frontier
                       unless (null f)
                         append (loopmove-if
                                 (lambda (x)
                                   (or (= 9 (mref map (first x) (second x)))
                                       (loopind x basin)))
                                 (neighbors f map))
                       ;; Yes, I've given up on replacing pushnew.
                         and do (pushnew f basin :test #'equal)))
        finally (return basin)))

(defun basins (map)
  (loop with map = map
        with low-points = (low-points map)
        with frontiers = (loopcar #'list low-points)
        with basins = (loopcar (lambda (f) (drain-frontier f map)) frontiers)
        initially (return basins)))

(defun d9-2 ()
  "TIL that
- `loop' imposes a C-like programming style with a
  declarations-(iteration|condition)-return structure.
- Array crunching is quirky to do in any language, including CL.

I've been lucky with my puzzle input today, because I'm not checking
for local optima in my solution and a good puzzle input would've
revealed that."
  (loop with map = (heightmap)
        with basins = (basins map)
        with max = 0 and second-max and third-max
        for basin in basins
        if (> (length basin) max)
          do (setf third-max second-max
                   second-max max
                   max (length basin))
        else if (> (length basin) second-max)
               do (setf third-max second-max
                        second-max (length basin))
        else if (> (length basin) third-max)
               do (setf third-max (length basin))
        finally (return (* max second-max third-max))))
