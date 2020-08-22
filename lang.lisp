(defpackage lang
  (:use cl)
  (:export emit emit-brint emit-push
           new-bool-val new-text new-vm
	   pop-val  push-val
	   *text*
	   val-bool to-lisp
	   val-eq *vm*))

(in-package lang)

(defvar *vm*)
(defvar *text*)

(struct:define vm _
  (stack vector :init (make-array 0 :adjustable t)))

(defun new-vm ()
  ($vm))

(defun push-val (val &key (vm *vm*))
  (vector-push-extend val ($vm-stack vm)))

(defun pop-val (&key (vm *vm*))
  (vector-pop ($vm-stack vm)))

(struct:define text _
  (vm vm)
  (ops vector :init (make-array 0 :adjustable t)))

(defun new-text (&key (vm *vm*))
  ($text :vm vm))

(defmethod emit (op &key (out *text*))
  (vector-push-extend op ($text-ops out)))

(defmethod to-lisp (&key (in *text*) out)
  (eval `(lambda ()
	     (tagbody
		,@(reduce (lambda (out op)
			    (op-lisp op :vm ($text-vm in) :out out))
			  ($text-ops in)
			  :initial-value out)))))

(struct:define val _
  (data t))

(defmethod val-eq (x y)
  nil)

(struct:define bool-val val)

(defun new-bool-val (data)
  ($bool-val :data data))

(defmethod val-bool ((val bool-val))
  ($val-data val))

(defmethod val-eq ((x bool-val) (y bool-val))
  (eq ($val-data x) ($val-data y)))
  
(struct:define op _)

(struct:define brint-op op
  (lhs integer)
  (true-label integer)
  (false-label integer))

(defmethod emit-brint (lhs true-label false-label &key (out *text*))
  (emit ($brint-op :lhs lhs :true-label true-label :false-label false-label) :out out))

(defmethod op-lisp ((op brint-op) &key out (vm *vm*))
  (cons `(go (if (= (pop-val :vm ,vm)) ,($brint-op-true-label op) ,($brint-op-false-label op))) out))

(struct:define push-op op
  (val val))

(defmethod emit-push (val &key (out *text*))
  (emit ($push-op :val val) :out out))

(defmethod op-lisp ((op push-op) &key out (vm *vm*))
  (cons `(push-val ,($push-op-val op) :vm ,vm) out))
