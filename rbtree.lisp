(defpackage rbtree
  (:use cl)
  (:import-from compare compare-fixnum)
  (:import-from util while)
  (:export new add-node remove-node find-key size))

(in-package rbtree)

;;TODO
;;replace with-slots
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

(defun new (compare)
  ($tree :compare compare))

(defun add-node (tree key val)
  (declare (type tree tree))
  (labels ((rec (node key val)
	     (declare (type (or node null) node))
	     (if node
		 (progn
		   (ecase (funcall ($tree-compare tree) key ($node-key node))
		     (:lt (with-slots (left) node
			    (declare (type (or node null) left))
			    (multiple-value-bind (l ok) (rec left key val)
			      (setf left l)
			      (values (fix node) ok))))
		     (:gt (with-slots (right) node
			    (declare (type (or node null) right))
			    (multiple-value-bind (r ok) (rec right key val)
			      (setf right r)
			      (values (fix node) ok))))
		     (:eq (values node nil))))
		 (progn
		   (incf ($tree-size tree))
		   (values ($node :key key :value val) t)))))
    (with-slots (root) tree
      (multiple-value-bind (new-root ok) (rec root key val)
	(when ok
	  (setf ($node-red? new-root) nil
		root new-root)
	  t)))))

(defun remove-node (tree key)
  (labels ((move-red-left (node)
	     (flip node)
	     (with-slots (right) node
	       (when (red? ($node-left right))
		 (setf right (rotr right)
		       node (rotl node))
		 (flip node)))
	     node)
		 
	   (remove-min (node)
	     (if (null ($node-left node))
		 (values nil node)
		 (progn
		   (with-slots (left) node
		     (when (and (not (red? left))
				(not (red? ($node-left left))))
		       (setf node (move-red-left node))))
		   (multiple-value-bind (new-left new-node) (remove-min ($node-left node))
		     (setf ($node-left node) new-left)
		     (values (fix node) new-node)))))
	   
	   (rec (node key)
	     (with-slots (compare) tree
	       (declare (type function compare))
	       (if node
		   (if (eq (funcall compare key ($node-key node)) :lt)
		       (progn
			 (with-slots (left) node
			   (when (and (not (red? left))
				      (not (red? ($node-left left))))
			     (setf node (move-red-left node))))
			 (with-slots (left) node
			   (multiple-value-bind (new-left val) (rec left key)
			     (setf left new-left)
			     (values (fix node) val))))
		       (progn
			 (when (red? ($node-left node))
			   (setf node (rotr node)))
			 (if (and (eq (funcall compare key ($node-key node)) :eq)
				  (null ($node-right node)))
			     (progn
			       (decf ($tree-size tree))
			       (values nil ($node-value node)))
			     (progn
			       (with-slots (left right) node
				 (when (and right
					    (not (red? right))
					    (not (red? ($node-left right))))
				   (flip node)
				   (when (red? ($node-left left))
				     (setf node (rotr node))
				     (flip node))))
			       (if (eq (funcall compare key ($node-key node)) :eq)
				   (progn
				     (let ((l ($node-left node))
					   (val ($node-value node)))
				       (multiple-value-bind (r new-node) (remove-min ($node-right node))
					 (setf node new-node
					       ($node-left node) l
					       ($node-right node) r))
				       (decf ($tree-size tree))
				       (values (fix node) val)))
				   (with-slots (right) node
				     (multiple-value-bind (new-right val) (rec right key)
				       (setf right new-right)
				       (values (fix node) val))))))))
		       (values node nil)))))
    (with-slots (root) tree
      (multiple-value-bind (new-root val) (rec root key)
	(setf root new-root)
	(when root
	  (setf ($node-red? new-root) nil))
	val))))
	  
(defun find-key (tree key)
  (declare (type tree tree))
  (let ((node ($tree-root tree)))
    (declare (type (or node null) node))
    (with-slots (compare) tree
      (while node
	(ecase (funcall compare key ($node-key node))
	  (:lt
	   (setf node ($node-left node)))
	  (:gt
	   (setf node ($node-right node)))
	  (:eq
	   (return)))))
    (and node ($node-value node))))

(defun red? (node)
  (declare (type (or node null) node))
  (and node ($node-red? node)))

(defun rotl (node)
  (with-slots (right red?) node
   (let ((r right))
      (declare (type (or node null) right) (type node r))
      (setf right ($node-left r)
	    ($node-left r) node
	    ($node-red? r) red?
	    red? t)
      r)))

(defun rotr (node)
  (declare (type node node))
  (with-slots (left red?) node
    (let ((l left))
      (declare (type (or node null) left) (type node l))
      (setf left ($node-right l)
	    ($node-right l) node
	    ($node-red? l) red?
	    red? t)
    l)))

(defun flip (node)
  (declare (type node node))
  (with-slots (left right red?) node
    (declare (type node left right) (type boolean red?))
    (setf red? (not red?)
	  ($node-red? left) (not ($node-red? left))
	  ($node-red? right) (not ($node-red? right)))))

(defun fix (node)
  (declare (type node node))
  (when (red? ($node-right node))
    (setf node (rotl node)))
  (when (and (red? ($node-left node)) (red? ($node-left ($node-left node))))
    (setf node (rotr node)))
  (when (and (red? ($node-left node)) (red? ($node-right node)))
    (flip node))
  node)

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
    (assert (equal (find-key tree 1) 'foo))
    (assert (null (find-key tree 2)))
    (assert (equal (find-key tree 3) 'baz))
    (assert (equal (find-key tree 4) 'qux))))

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
