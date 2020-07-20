(defpackage compare
  (:use cl)
  (:export compare-fixnum))

(in-package compare)

(declaim (optimize (speed 3) (safety 0)))

(defun compare-fixnum (x y)
  (declare (type fixnum x y))
  (cond
    ((< x y) :lt)
    ((> x y) :gt)
    (t :eq)))
