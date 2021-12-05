(in-package #:aoc-in-loop)

(defun commands ()
  (loop :for raw-command
          :in (uiop:read-file-lines (asdf:system-relative-pathname :aoc-in-loop "input/d2.in"))
        :for (command raw-quantity) := (uiop:split-string raw-command)
        :for quantity := (parse-integer raw-quantity)
        :collect (list command quantity)))

(export 'd2-1)
(defun d2-1 ()
  (loop :with horisontal := 0
        :with vertical := 0
        :for (command quantity) :in (commands)
        :if (equal "up" command)
          :do (decf vertical quantity)
        :else :if (equal "down" command)
          :do (incf vertical quantity)
        :else :do (incf horisontal quantity)
        :finally (return (* horisontal vertical))))

(export 'd2-2)
(defun d2-2 ()
  (loop :with aim := 0
        :with horisontal := 0
        :with vertical := 0
        :for (command quantity) :in (commands)
        :if (equal "up" command)
          :do (decf aim quantity)
        :else :if (equal "down" command)
                :do (incf aim quantity)
        :else :do (progn (incf horisontal quantity)
                         (incf vertical (* aim quantity)))
        :finally (return (* horisontal vertical))))
