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

(defun loop-diff (l1 l2)
  "A loop-based copy of `set-difference'."
  (loop for e1 in l1
        unless (loopind e1 l2)
          collect e1))

(defun loop-unite (l1 l2)
  "A loop-based copy of `union'."
  (loop repeat 1
        append (loop for e in l1
                     unless (loopind e result1)
                       collect e into result1
                     finally (return result1))
          into result
        append (loop for e in l2
                     unless (or (loopind e result)
                                (loopind e result2))
                       collect e into result2
                     finally (return result2))
          into result
        finally (return result)))

(defun loop-intersect (l1 l2)
  "A loop-based copy of `intersection'."
  (loop for e in l1
        when (loopind e l2)
          collect e))
