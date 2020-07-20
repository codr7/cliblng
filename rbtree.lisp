(defpackage rbtree
  (:use cl)
  (:import-from compare compare-fixnum)
  (:import-from util while)
  (:export new add-node remove-node find-key size))

(in-package rbtree)

;;TODO
;;simplify nested slot-value calls
;;;add slot macro
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
  (new-tree :compare compare))

(defun add-node (tree key val)
  (declare (type tree tree))
  (labels ((rec (node key val)
	     (declare (type (or node null) node))
	     (if node
		 (progn
		   (ecase (funcall (slot-value tree 'compare) key (slot-value node 'key))
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
		   (incf (slot-value tree 'size))
		   (values (new-node :key key :value val) t)))))
    (with-slots (root) tree
      (multiple-value-bind (new-root ok) (rec root key val)
	(when ok
	  (setf (slot-value new-root 'red?) nil
		root new-root)
	  t)))))

(defun remove-node (tree key)
  (labels ((move-red-left (node)
	     (flip node)
	     (with-slots (right) node
	       (when (red? (slot-value right 'left))
		 (setf right (rotr right)
		       node (rotl node))
		 (flip node)))
	     node)
		 
	   (remove-min (node)
	     (if (null (slot-value node 'left))
		 (values nil node)
		 (progn
		   (with-slots (left) node
		     (when (and (not (red? left))
				(not (red? (slot-value left 'left))))
		       (setf node (move-red-left node))))
		   (multiple-value-bind (new-left new-node) (remove-min (slot-value node 'left))
		     (setf (slot-value node 'left) new-left)
		     (values (fix node) new-node)))))
	   
	   (rec (node key)
	     (with-slots (compare) tree
	       (declare (type function compare))
	       (if node
		   (if (eq (funcall compare key (slot-value node 'key)) :lt)
		       (progn
			 (with-slots (left) node
			   (when (and (not (red? left))
				      (not (red? (slot-value left 'left))))
			     (setf node (move-red-left node))))
			 (with-slots (left) node
			   (multiple-value-bind (new-left val) (rec left key)
			     (setf left new-left)
			     (values (fix node) val))))
		       (progn
			 (when (red? (slot-value node 'left))
			   (setf node (rotr node)))
			 (if (and (eq (funcall compare key (slot-value node 'key)) :eq)
				  (null (slot-value node 'right)))
			     (progn
			       (decf (the fixnum (slot-value tree 'size)))
			       (values nil (slot-value node 'value)))
			     (progn
			       (with-slots (left right) node
				 (when (and right
					    (not (red? right))
					    (not (red? (slot-value right 'left))))
				   (flip node)
				   (when (red? (slot-value left 'left))
				     (setf node (rotr node))
				     (flip node))))
			       (if (eq (funcall compare key (slot-value node 'key)) :eq)
				   (progn
				     (let ((l (slot-value node 'left))
					   (val (slot-value node 'value)))
				       (multiple-value-bind (r new-node) (remove-min (slot-value node 'right))
					 (setf node new-node
					       (slot-value node 'left) l
					       (slot-value node 'right) r))
				       (decf (the fixnum (slot-value tree 'size)))
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
	  (setf (slot-value new-root 'red?) nil))
	val))))
	  
(defun find-key (tree key)
  (declare (type tree tree))
  (let ((node (slot-value tree 'root)))
    (declare (type (or node null) node))
    (with-slots (compare) tree
      (while node
	(ecase (funcall compare key (slot-value node 'key))
	  (:lt
	   (setf node (slot-value node 'left)))
	  (:gt
	   (setf node (slot-value node 'right)))
	  (:eq
	   (return)))))
    (when node
      (slot-value node 'value))))

(defun red? (node)
  (declare (type (or node null) node))
  (and node (slot-value node 'red?)))

(defun rotl (node)
  (with-slots (right red?) node
   (let ((r right))
      (declare (type (or node null) right) (type node r))
      (setf right (slot-value r 'left)
	    (slot-value r 'left) node
	    (slot-value r 'red?) red?
	    red? t)
      r)))

(defun rotr (node)
  (declare (type node node))
  (with-slots (left red?) node
    (let ((l left))
      (declare (type (or node null) left) (type node l))
      (setf left (slot-value l 'right)
	    (slot-value l 'right) node
	    (slot-value l 'red?) red?
	    red? t)
    l)))

(defun flip (node)
  (declare (type node node))
  (with-slots (left right red?) node
    (declare (type node left right) (type boolean red?))
    (setf red? (not red?)
	  (slot-value left 'red?) (not (slot-value left 'red?))
	  (slot-value right 'red?) (not (slot-value right 'red?)))))

(defun fix (node)
  (declare (type node node))
  (when (red? (slot-value node 'right))
    (setf node (rotl node)))
  (when (and (red? (slot-value node 'left)) (red? (slot-value (slot-value node 'left) 'left)))
    (setf node (rotr node)))
  (when (and (red? (slot-value node 'left)) (red? (slot-value node 'right)))
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
