(defpackage compare
  (:use cl)
  (:export compare-int))

(in-package compare)

(defun compare-int (x y)
  (declare (type integer x y))
  (cond
    ((< x y) :lt)
    ((> x y) :gt)
    (t :eq)))
