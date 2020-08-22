(defpackage lang-tests
  (:use cl)
  (:export run))

(in-package lang-tests)

(defun run ()
  (let* ((lang:*vm* (lang:new-vm))
	 (lang:*text* (lang:new-text)))
    (lang:emit-push (lang:new-bool-val t))
    (funcall (lang:to-lisp))
    (assert (lang:val-eq (lang:pop-val) (lang:new-bool-val t)))))
