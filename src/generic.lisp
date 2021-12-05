(in-package :aoc-in-loop)

(defun loopmove-if (predicate list)
  "A loop-based copy of `remove-if'."
  (loop :for l :in list
        :unless (funcall predicate l)
          :collect l))

(defun loopcar (function list)
  "A loop-based copy of `mapcar'."
  (loop :for l :in list
        :collect (funcall function l)))

(defun loopind (item list)
  "A loop-based copy of `find'."
  (loop :for e :in list
        :when (equal item e)
          :do (return t)
        :finally (return nil)))

(defun loopverse (list)
  "A loop-based copy of `reverse'."
  (loop :for i :from (1- (length list)) :downto 0
        :collect (elt list i)))
