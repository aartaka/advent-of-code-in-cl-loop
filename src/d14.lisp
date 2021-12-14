(in-package :aoc-in-loop)

(defun sequences ()
  (loop with file = (asdf:system-relative-pathname :aoc-in-loop "input/d14.in")
        with template = (coerce (uiop:read-file-line file) 'list)
        with raw-rules = (subseq (uiop:read-file-lines file) 2)
        with rules = (make-hash-table :test 'equalp)
        for raw-rule in raw-rules
        do (setf (gethash (coerce (subseq raw-rule 0 2) 'list) rules)
                 (elt raw-rule (1- (length raw-rule))))
        finally (return (list template rules))))

(defun pair-insert (&optional (steps 10))
  (loop with sequence-data = (sequences)
        with sequence = (first sequence-data)
        with rules = (second sequence-data)
        for step from 1 upto steps
        do (setf sequence
                 (loop for (a b . rest) on sequence
                       append (list a (gethash (list a b) rules)) into new-seq
                       if (null rest)
                         collect b into new-seq
                         and do (return new-seq)))
        finally (return sequence)))

(defun d14-1 ()
  (loop with ten-days-result = (pair-insert)
        with freqs = (make-hash-table)
        for char in ten-days-result
        do (if (gethash char freqs)
               (incf (gethash char freqs))
               (setf (gethash char freqs) 1))
        finally (return (loop for freq being the hash-value of freqs
                              maximize freq into max
                              minimize freq into min
                              finally (return (- max min))))))

(defun d14-2 ()
  (loop with forty-days-result = (pair-insert 40)
        with freqs = (make-hash-table)
        for char in forty-days-result
        do (if (gethash char freqs)
               (incf (gethash char freqs))
               (setf (gethash char freqs) 1))
        finally (return (loop for freq being the hash-value of freqs
                              maximize freq into max
                              minimize freq into min
                              finally (return (- max min))))))
