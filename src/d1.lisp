(in-package #:aoc-in-loop)

(defun measurements ()
  (loop :for raw-measurement
          :in (uiop:read-file-lines (asdf:system-relative-pathname :aoc-in-loop "input/d1.in"))
        :collect (parse-integer raw-measurement)))

(export 'd1-1)
(defun d1-1 ()
  (loop :with increasing-measurements := 0
        :with previous-measurement := nil
        :for measurement :in (measurements)
        :when (and previous-measurement
                   (> measurement previous-measurement))
          :do (incf increasing-measurements)
        :do (setf previous-measurement measurement)
        :finally (return increasing-measurements)))

(export 'd1-2)
(defun d1-2 ()
  (loop :with increasing-window-measurements := 0
        :with previous-window-measurement := nil
        :for (one two three) :on (measurements)
        :for window-measurement
          := (ignore-errors (+ one two three))
        :collect window-measurement
        :until (null window-measurement)
        :when (and previous-window-measurement
                   (> window-measurement previous-window-measurement))
          :do (incf increasing-window-measurements)
        :do (setf previous-window-measurement window-measurement)
        :finally (return increasing-window-measurements)))
