(in-package :aoc-in-loop)

(defun chiton-risks ()
  (read-matrix (asdf:system-relative-pathname :aoc-in-loop "input/d15-sample.in")))

(defun cost (map path)
  (loop for (x y) in path
        sum (elt (elt map x) y)))

(defun path (map &optional (start '(0 0))
                   (end (list (1- (length map)) (1- (length (first map))))))
  "This is basically a stripped-down version of A-star."
  (loop with frontier = (list (list start))
        with paths = '()
        while frontier
        do (loop with lowest-cost-option = )))
