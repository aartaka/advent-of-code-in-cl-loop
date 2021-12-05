(in-package :aoc-in-loop)

(defun get-bingo ()
  (loop :with lines := (uiop:read-file-lines (asdf:system-relative-pathname :aoc-in-loop "input/d4-sample.in"))
        :with numbers := (loopcar #'parse-integer (uiop:split-string (first lines) :separator '(#\,)))
        :with boards := (loop :for (l1 l2 l3 l4 l5) :on (loopmove-if #'uiop:emptyp (rest lines))
                                :by (lambda (list) (cddr (cdddr list)))
                              :collect (loop :for l :in (list l1 l2 l3 l4 l5)
                                             :collect (loopcar #'parse-integer
                                                               (loopmove-if #'uiop:emptyp (uiop:split-string l)))))
        :initially (return (list numbers boards))))

(defun winning-p (board numbers)
  (loop :with row-win
          := (loop :for row :in board
                   :when (loop :for e :in row
                               :when (not (loopind e numbers))
                                 :do (return nil)
                               :finally (return t))
                     :do (return t)
                   :finally (return nil))
        :with column-win
          := (loop :for column-index :below 5
                   :when (loop :for row-index :below 5
                               :when (not (loopind (elt (elt board row-index) column-index) numbers))
                                 :do (return nil)
                               :finally (return t))
                     :do (return t)
                   :finally (return nil))
        :initially (return (or row-win column-win))))

(defun final-score (board numbers)
  (loop :for row :in board
        :sum (loop :for num :in row
                   :when (not (loopind num numbers))
                     :sum num :into sum
                   :finally (return (* sum (first (last numbers)))))))

(defun number-lists (numbers)
  (loop :for ns :on (loopverse numbers)
        :collect ns :into nsx
        :finally (return (loop :for n :in (loopverse nsx)
                               :collect (loopverse n)))))

(defun d4-1 ()
  (loop :named d4
        :with bingo := (get-bingo)
        :with nums := (first bingo)
        :with boards := (second bingo)
        :with number-lists := (number-lists nums)
        :for numbers :in number-lists
        :do (loop :for board :in boards
                  :when (winning-p board numbers)
                    :do (return-from d4 (final-score board numbers)))))

(defun d4-2 ()
  (loop :named d4
        :with bingo := (get-bingo)
        :with nums := (first bingo)
        :with boards := (second bingo)
        :with number-lists := (number-lists nums)
        :for numbers :in number-lists
        :when (and (= (length boards) 1)
                   (winning-p (first boards) numbers))
          :do (return-from d4 (final-score (first boards) numbers))
        :do (setf boards
                  (loop :for board :in boards
                        :unless (winning-p board numbers)
                          :collect board))))
