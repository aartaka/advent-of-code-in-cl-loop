(in-package :aoc-in-loop)

(defun all-upcase-p (string)
  (not (loop for char across string
             thereis (lower-case-p char))))

(defun caves ()
  (loop with cave-table = (make-hash-table :test 'equal)
        for raw-cave in (uiop:read-file-lines
                         (asdf:system-relative-pathname :aoc-in-loop "input/d12.in"))
        for (start end) = (uiop:split-string raw-cave :separator '(#\-))
        unless (gethash start cave-table)
          do (setf (gethash start cave-table)
                   (make-hash-table :test 'equal))
        unless (gethash end cave-table)
          do (setf (gethash end cave-table)
                   (make-hash-table :test 'equal))
        do (setf (gethash start (gethash end cave-table)) t
                 (gethash end (gethash start cave-table)) t)
        finally (return cave-table)))

(defun hash-keys (hash)
  (when hash
    (loop for key being the hash-key of hash
          collect key)))

(defvar *choices* (make-hash-table :test 'equalp))

(defun visitable-p (choice visited can-visit-extra-small-cave)
  (and (string/= "start" choice)
       (or (all-upcase-p choice)
           can-visit-extra-small-cave
           (not (loopind choice visited)))))

(defun paths (&optional
                (cave-map (caves))
                (visited '("start"))
                can-visit-extra-small-cave)
  (loopmove-if
   (lambda (path) (string/= "end" (first (reverse path))))
   (loop with choices = (loop for choice in (hash-keys
                                             (gethash (first (reverse visited)) cave-map))
                              when (visitable-p choice visited can-visit-extra-small-cave)
                                collect choice)
         with whatever = (setf (gethash visited *choices*)
                               choices)
         if (null choices)
           do (return (list visited))
         else if (and (= 1 (length choices))
                      (not (string= "end" (first choices))))
                do (return (paths cave-map (append visited choices)
                                  (if (or (all-upcase-p (first choices))
                                          (not (loopind (first choices) visited)))
                                      can-visit-extra-small-cave
                                      nil)))
         else do (return (loop for ch in choices
                               if (string= "end" ch)
                                 append (list (append visited (list ch)))
                               else
                                 append (paths cave-map (append visited (list ch))
                                               (if (or (all-upcase-p ch)
                                                       (not (loopind ch visited)))
                                                   can-visit-extra-small-cave
                                                   nil)))))))

(export 'd12-1)
(defun d12-1 ()
  (length (paths)))

(export 'd12-2)
(defun d12-2 ()
  (length (paths (caves) '("start") t)))
