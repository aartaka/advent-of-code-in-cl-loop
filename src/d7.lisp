(in-package :aoc-in-loop)

(defun submarines ()
  (loop for raw-submarine in (uiop:split-string
                              (uiop:read-file-line
                               (asdf:system-relative-pathname :aoc-in-loop "input/d7.in"))
                              :separator '(#\,))
        collect (parse-integer raw-submarine)))

(defun mean (list)
  (loop for e in list
        sum e into sum
        finally (return (round (/ sum (length list))))))

(defun standard-deviation (list)
  (loop with mean = (mean list)
        for e in list
        sum (expt (- e mean) 2) into sum-of-squares
        finally (return (sqrt (/ sum-of-squares (1- (length list)))))))

(defun sigma-fuel-function (n)
  (/ (+ (expt n 2) n) 2))

(defun d7 (&optional (submarines (submarines)) (fuel-function #'identity))
  "TIL that
- Knowing statistics is helpful.
- Mean and deviation still help, even in highly dispersed samples.
- It seems quite unavoidable having `finally (return...)' clause in any non-trivial LOOP.
  - What a shame.

How does one optimize statistical calculations?"
  (loop with mean = (mean submarines)
        with delta = (round (/ (standard-deviation submarines) 2))
        for alignment from (max 0 (- mean delta))
          upto (+ mean delta)
        minimize (loop for sub in submarines
                       sum (funcall fuel-function (abs (- sub alignment))))))
