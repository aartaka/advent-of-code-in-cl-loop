(in-package :aoc-in-loop)

(defun chiton-risks ()
  (read-matrix (asdf:system-relative-pathname :aoc-in-loop "input/d15-sample.in")))

(defun cost (map path)
  (loop for (x y) in path
        sum (elt (elt map x) y)))

(defun 2-neighbors (map point)
  (append
   (when (< (1+ (second point)) (length (first map)))
     (list (list (first point) (1+ (second point)))))
   (when (< (1+ (first point)) (length map))
     (list (list (1+ (first point)) (second point))))))

(defun path (map &optional (start '(0 0))
                 (end (list (1- (length map)) (1- (length (first map))))))
  "This is basically a dumbed-down version of A-star."
  (loop named a-star
        with frontier = (list (list start))
        with paths = '()
        while frontier
        for lowest-cost-option
          = (loop with min-cost = most-positive-fixnum
                  with min-index = nil
                  for path in frontier
                  for i below (length frontier)
                  when (< (cost map path) min-cost)
                    do (setf min-cost (cost map path)
                             min-index i)
                  finally (return (let ((min (elt frontier min-index)))
                                    (setf frontier (append (subseq frontier 0 min-index)
                                                           (subseq frontier (1+ min-index))))
                                    min)))
        for prev-step = (car (last lowest-cost-option))
        do (loop for neigh in (2-neighbors map prev-step)
                 for path = (append lowest-cost-option (list neigh))
                 if (equalp neigh end)
                   do (return-from a-star path)
                 else do (push path frontier))))

(defun d15-1 ()
  (loop with map = (chiton-risks)
        with path = (path map)
        initially (return (cost map (cdr path)))))
