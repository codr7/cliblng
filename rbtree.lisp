(defpackage rbtree
  (:use cl)
  (:import-from compare compare-fixnum)
  (:import-from util nor while)
  (:export new add-node remove-node find-key size))

(in-package rbtree)

;;TODO
;;replace node.key with tree.key function
;;;use val default
;;;update tests/benchmark

(declaim (optimize (speed 3) (safety 0)))

(struct:define node _
  (key t)
  (value t)
  (left (or node null))
  (right (or node null))
  (red? boolean :init t))

(struct:define tree _
  (compare function)
  (root (or node null))
  (size fixnum :init 0 :read _))

(defmacro rotlf (node)
  `(setf ,node (rotl ,node)))

(defmacro rotrf (node)
  `(setf ,node (rotr ,node)))

(defun new (compare)
  ($tree :compare compare))

(defun compare (tree x y)
  (funcall ($tree-compare tree) x y))

(defun red? (node)
  (declare (type (or node null) node))
  (and node ($node-red? node)))

(defun rotl (node)
  (declare (type node node))
  (let ((r ($node-right node)))
    (setf ($node-right node) ($node-left r)
	  ($node-left r) node
	  ($node-red? r) ($node-red? node)
	  ($node-red? node) t)
    r))

(defun rotr (node)
  (declare (type node node))
  (let ((l ($node-left node)))
    (setf ($node-left node) ($node-right l)
	  ($node-right l) node
	  ($node-red? l) ($node-red? node)
	  ($node-red? node) t)
    l))

(defun flip (node)
  (declare (type node node))
  (setf ($node-red? node) (not ($node-red? node))
	($node-red? ($node-left node)) (not ($node-red? ($node-left node)))
	($node-red? ($node-right node)) (not ($node-red? ($node-right node)))))

(defun fix (node)
  (declare (type node node))
  (when (red? ($node-right node))
    (rotlf node))
  (when (and (red? ($node-left node)) (red? ($node-left ($node-left node))))
    (rotrf node))
  (when (and (red? ($node-left node)) (red? ($node-right node)))
    (flip node))
  node)

(defun add-node (tree key val)
  (declare (type tree tree))
  (labels ((rec (node key val)
	     (declare (type (or node null) node))
	     (if node
		 (progn
		   (ecase (compare tree key ($node-key node))
		     (:lt (multiple-value-bind (l ok) (rec ($node-left node) key val)
			    (setf ($node-left node) l)
			    (values (fix node) ok)))
		     (:gt (multiple-value-bind (r ok) (rec ($node-right node) key val)
			    (setf ($node-right node) r)
			    (values (fix node) ok)))
		     (:eq (values node nil))))
		 (progn
		   (incf ($tree-size tree))
		   (values ($node :key key :value val) t)))))
    (multiple-value-bind (new-root ok) (rec ($tree-root tree) key val)
      (when ok
	(setf ($node-red? new-root) nil
	      ($tree-root tree) new-root)
	t))))

(defmacro red-leftf (node)
  `(setf ,node (move-red-left ,node)))

(defun red-left (node)
  (flip node)
  (when (red? ($node-left ($node-right node)))
    (rotrf ($node-right node))
    (rotlf node)
    (flip node))
  node)

(defun remove-min (node)
  (if (null ($node-left node))
      (values nil node)
      (progn
	(when (nor (red? ($node-left node))
		   (red? ($node-left ($node-left node))))
	  (red-leftf node))
	(multiple-value-bind (new-left new-node) (remove-min ($node-left node))
	  (setf ($node-left node) new-left)
	  (values (fix node) new-node)))))

(defun remove-node (tree key)
  (labels ((rec (node key)
	     (if node
		 (if (eq (compare tree key ($node-key node)) :lt)
		     (progn
		       (when (nor (red? ($node-left node))
				  (red? ($node-left ($node-left node))))
			 (red-leftf node))
		       (multiple-value-bind (new-left val) (rec ($node-left node) key)
			 (setf ($node-left node) new-left)
			 (values (fix node) val)))
		     (progn
		       (when (red? ($node-left node))
			 (rotrf node))
		       (if (and (eq (compare tree key ($node-key node)) :eq)
				(null ($node-right node)))
			   (progn
			     (decf ($tree-size tree))
			     (values nil ($node-value node)))
			   (progn
			     (when (and ($node-right node)
					(nor (red? ($node-right node))
					     (red? ($node-left ($node-right node)))))
			       (flip node)
			       (when (red? ($node-left ($node-left node)))
				 (rotrf node)
				 (flip node)))
			     (if (eq (compare tree key ($node-key node)) :eq)
				 (progn
				   (let ((l ($node-left node))
					 (val ($node-value node)))
				     (multiple-value-bind (r new-node) (remove-min ($node-right node))
				       (setf node new-node
					     ($node-left node) l
					     ($node-right node) r))
				     (decf ($tree-size tree))
				     (values (fix node) val)))
				 (multiple-value-bind (new-right val) (rec ($node-right node) key)
				   (setf ($node-right node) new-right)
				   (values (fix node) val)))))))
		 (values node nil))))
    (multiple-value-bind (new-root val) (rec ($tree-root tree) key)
      (when new-root
	(setf ($node-red? new-root) nil))
      (setf ($tree-root tree) new-root)
      val)))

(defun find-key (tree key)
  (declare (type tree tree))
  (let ((node ($tree-root tree)))
    (while node
      (ecase (compare tree key ($node-key node))
	(:lt
	 (setf node ($node-left node)))
	(:gt
	 (setf node ($node-right node)))
	(:eq
	 (return))))
    (and node ($node-value node))))

(defun run-tests ()
  (let ((tree (new #'compare-fixnum)))
    (assert (add-node tree 1 'foo))
    (assert (add-node tree 2 'bar))
    (assert (add-node tree 3 'baz))
    (assert (not (add-node tree 3 'qux)))
    (assert (add-node tree 4 'qux))
    (assert (eq (remove-node tree 2) 'bar))
    (assert (null (remove-node tree 2)))
    (assert (= (size tree) 3))
    (assert (eq (find-key tree 1) 'foo))
    (assert (null (find-key tree 2)))
    (assert (eq (find-key tree 3) 'baz))
    (assert (eq (find-key tree 4) 'qux))))

(defun run-benchmark ()
  (let ((max 1000000))
    (time
     (let ((tbl (make-hash-table)))
       (dotimes (i max)
	 (setf (gethash i tbl) i))
       (dotimes (i max)
	 (assert (= (gethash i tbl) i)))
       (dotimes (i max)
	 (assert (remhash i tbl)))))
    
    (time
     (let ((tree (new #'compare-fixnum)))
       (dotimes (i max)
	 (assert (add-node tree i i)))
       (dotimes (i max)
	 (assert (= (find-key tree i) i)))
       (dotimes (i max)
	 (assert (= (remove-node tree i) i)))))))
