(defpackage repl
  (:use cl)
  (:export start test))

(in-package repl)

(defmacro start ((&optional (in *standard-input*) (out *standard-output*)) &body body)
  `(flet ((r (spec &rest args)
	    (apply #'format ,out spec args)
	    (read-line ,in nil))
	  
	  (p (spec &rest args)
	    (apply #'format ,out spec args)
	    (terpri ,out)
	    (finish-output ,out)))
     (macrolet ((rl ((var spec &rest args) &body body)
		  `(tagbody
		    next
		      (let ((,var (apply #'r ,spec ,args)))
			(macrolet ((quit ()
				     `(go quit)))
			  ,@body))
		      (go next)
		    quit)))
       ,@body)))

(defun test ()
  (start (*standard-input* *standard-output*)
    (p "My REPL v1.0")
    (p "Press Return on empty line to quit.")
    (p "")
    
    (rl (in "$ ")
	(when (string= in "")
	  (quit))
	
	(p "Unknown command: ~a" in))))
