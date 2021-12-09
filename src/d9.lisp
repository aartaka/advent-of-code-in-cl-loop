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
        when (and (>= (1- (length map))         (+ xdiff x) 0)
                  (>= (1- (length (first map))) (+ ydiff y) 0))
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

(defun basins (map)
  (loop with map = (heightmap)
        with low-points = (low-points map)
        with frontiers = (loopcar #'list low-points)
        with basins = (make-list (length frontiers))
        while frontiers
        do (setf frontiers
                 (loop for frontier in frontiers
                       collect (loop for f in frontier
                                     for b in basins
                                     unless (null f)
                                       collect (loopmove-if
                                                (lambda (x)
                                                  (or (= 9 (mref map (first x) (second x)))
                                                      (loop for basin in basins
                                                            when (loopind x basin)
                                                              do (return t)
                                                            finally (return nil))))
                                                (neighbors f map))
                                       and do (push f b))))))
