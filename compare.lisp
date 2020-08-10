(defpackage compare
  (:use cl)
  (:export compare))

(in-package compare)

(declaim (optimize (speed 3) (safety 0)))

(defmethod compare ((x fixnum) (y fixnum))
  (declare (type fixnum x y))
  (cond
    ((< x y) :lt)
    ((> x y) :gt)
    (t :eq)))
