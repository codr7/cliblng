(defpackage vm-tests
  (:use cl)
  (:export run))

(in-package vm-tests)

(defun run ()
  (let ((vm (vm:new))
	(c (vm:new-code)))
    (vm:o-push (vm:new-bool-value t) c)
    (funcall (vm:to-lisp c vm nil))
    (assert (vm:value= (vm:pop-value vm) (vm:new-bool-value t)))))
    
    
