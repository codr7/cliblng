(defpackage lang
  (:use cl)
  (:export bool-value code
	   new-bool-value new-code new-vm
	   o-push o-when
	   op
	   pop-value push-op push-value
	   to-bool to-lisp
	   value= vm))

(in-package lang)

(struct:define vm _
  (stack vector :init (make-array 0 :adjustable t)))

(defun new-vm ()
  ($vm))

(defun push-value (val vm)
  (vector-push-extend val ($vm-stack vm)))

(defun pop-value (vm)
  (vector-pop ($vm-stack vm)))

(struct:define code _
  (ops vector :init (make-array 0 :adjustable t)))

(defun new-code ()
  ($code))

(defun push-op (op code)
  (vector-push-extend op ($code-ops code)))

(defmethod to-lisp ((code code) vm out)
  (eval `(lambda ()
	     (tagbody
		,@(reduce (lambda (out op)
			    (to-lisp op vm out))
			  ($code-ops code)
			  :initial-value out)))))

(struct:define value _
  (data t))

(defmethod value= (x y)
  nil)

(struct:define bool-value value)

(defun new-bool-value (data)
  ($bool-value :data data))

(defmethod value= ((x bool-value) (y bool-value))
  (eq ($value-data x) ($value-data y)))
  
(defmethod to-bool ((val bool-value))
  ($value-data val))

(struct:define op _)

(struct:define o-push op
  (val value))

(defun o-push (val code)
  (push-op ($o-push :val val) code))

(defmethod to-lisp ((op o-push) vm out)
  (cons `(push-value ,($o-push-val op) ,vm) out))

(struct:define o-when op
  (label integer))

(defun o-when (label code)
  (push-op ($o-when :label label) code))

(defmethod to-lisp ((op o-when) vm out)
  (cons `(when (to-bool (pop-value ,vm)) (go ,($o-when-label op))) out))

