(defpackage util
  (:use cl)
  (:export dohash nor while
	   kw sethash sym))

(in-package util)

(defmacro dohash ((k v tbl) &body body)
  (let (($i (gensym)) ($next (gensym)) ($ok (gensym)))
    `(with-hash-table-iterator (,$i ,tbl)
       (tagbody
	  ,$next
	  (multiple-value-bind (,$ok ,k ,v) (,$i)
	    (declare (ignorable ,k ,v))
	    (when ,$ok
	      ,@body
	            (go ,$next)))))))

(defmacro nor (&rest args)
  `(not (or ,@args)))

(defmacro while (cnd &body body)
  (let (($next (gensym)))
    `(block nil
       (tagbody
	  ,$next
	  (when ,cnd
	    ,@body
	    (go ,$next))))))

(defun kw (&rest args)
  (intern (with-output-to-string (out)
	    (dolist (a args)
	      (princ (if (stringp a) (string-upcase a) a) out)))
	  :keyword))

(defun sethash (key tbl val)
  (setf (gethash key tbl) val))

(defun sym (&rest args)
  (intern (with-output-to-string (out)
	    (dolist (a args)
	            (princ (if (stringp a) (string-upcase a) a) out)))))
