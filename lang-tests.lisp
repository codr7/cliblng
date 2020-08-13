(defpackage lang-tests
  (:use cl)
  (:export run))

(in-package lang-tests)

(defun run ()
  (let ((vm (lang:new-vm))
	(c (lang:new-code)))
    (lang:o-push (lang:new-bool-value t) c)
    (funcall (lang:to-lisp c vm nil))
    (assert (lang:value= (lang:pop-value vm) (lang:new-bool-value t)))))
