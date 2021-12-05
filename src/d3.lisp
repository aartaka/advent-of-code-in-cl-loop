(in-package #:aoc-in-loop)

(defun diagnostics ()
  (loop :for byte
          :in (uiop:read-file-lines (asdf:system-relative-pathname :aoc-in-loop "input/d3.in"))
        :collect (parse-integer byte :radix 2)
          :into diags
        :finally (return (list (length byte) diags))))

(defun most-common-bit (diagnostics mask)
  (loop :for byte :in diagnostics
        :count (not (zerop (logand mask byte))) :into ones
        :finally (return (if (>= ones (/ (length diagnostics) 2))
                             1 0))))

(defun least-common-bit (diagnostics mask)
  (loop :for byte :in diagnostics
        :count (not (zerop (logand mask byte))) :into ones
        :finally (return (if (>= ones (/ (length diagnostics) 2))
                             0 1))))

(export 'd3-1)
(defun d3-1 ()
  (loop :with (byte-length diagnostics) := (diagnostics)
        :for i :below byte-length
        :for mask := (expt 2 i)
        :for g-bit := (most-common-bit diagnostics mask)
        :sum (* g-bit mask) :into gamma
        :finally (return (* gamma (- (1- (expt 2 byte-length)) gamma)))))

(export 'd3-2)
(defun d3-2 ()
  (loop :with (byte-length diagnostics) := (diagnostics)
        :with o2-diagnostics := diagnostics
        :with o2-result := nil
        :with co2-diagnostics := diagnostics
        :with co2-result := nil
        :for i :from (1- byte-length) :downto 0
        :for mask := (expt 2 i)
        :for o2b := (most-common-bit o2-diagnostics mask)
        :for co2b := (least-common-bit co2-diagnostics mask)
        :do (setf o2-diagnostics
                  (loop :for o2 :in o2-diagnostics
                        :when (eq (zerop (logand mask o2)) (zerop o2b))
                          :collect o2))
        :do (setf co2-diagnostics
                  (loop :for co2 :in co2-diagnostics
                        :when (eq (plusp (logand mask co2)) (plusp co2b))
                          :collect co2))
        :when (and (null o2-result)
                   (= 1 (length o2-diagnostics)))
          :do (setf o2-result (first o2-diagnostics)
                    o2-diagnostics nil)
        :when (and (null co2-result)
                   (= 1 (length co2-diagnostics)))
          :do (setf co2-result (first co2-diagnostics)
                    co2-diagnostics nil)
        :finally (return (* o2-result co2-result))))
