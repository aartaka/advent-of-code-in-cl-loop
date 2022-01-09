(in-package #:aoc-in-loop)

(defun measurements ()
  (loop for raw-measurement
          in (uiop:read-file-lines (asdf:system-relative-pathname :aoc-in-loop "input/d1.in"))
        collect (parse-integer raw-measurement)))

(export 'd1-1)
(defun d1-1 ()
  ;; Thanks @vindarel!
  (loop for depth in (measuments)
        and prev = nil then depth
        when prev
          count (< prev depth)))

(export 'd1-2)
(defun d1-2 ()
  (loop with increasing-window-measurements = 0
        with previous-window-measurement = nil
        for (one two three) on (measurements)
        for window-measurement
          = (ignore-errors (+ one two three))
        collect window-measurement
        until (null window-measurement)
        when (and previous-window-measurement
                  (> window-measurement previous-window-measurement))
          do (incf increasing-window-measurements)
        do (setf previous-window-measurement window-measurement)
        finally (return increasing-window-measurements)))
