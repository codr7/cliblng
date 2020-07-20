(defpackage util
  (:use cl)
  (:export while
	   sym))

(in-package util)

(defmacro while (cnd &body body)
  (let (($next (gensym)))
    `(block nil
       (tagbody
	  ,$next
	  (when ,cnd
	    ,@body
	    (go ,$next))))))

(defun sym (&rest args)
  (intern (with-output-to-string (out)
	    (dolist (a args)
	            (princ (if (stringp a) (string-upcase a) a) out)))))
