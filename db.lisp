(defpackage db
  (:use cl compare)
  (:import-from rb new-tree tree)
  (:import-from util dohash kw let-when sethash sym)
  (:export add-record remove-record
           close-files
	   define define-table drop
	   exists?
	   field find-id find-key
	   index-key init
	   name new-record next-id
	   push-column push-index push-table
	   record= record-count remove-record root
	   store
	   table))

(in-package db)

(struct:define root _
  (path string)
  (tables list)
  (indexes list))

(struct:define table _
  (root root)
  (name string :read _)
  (columns vector :init (make-array 0 :adjustable t))
  (column-lookup hash-table :init (make-hash-table :test 'eq))
  (key-file (or stream null))
  (data-file (or stream null))
  (max-id integer :init 0)
  (records hash-table :init (make-hash-table :test 'eq))
  (indexes list))

(defun compare-key (x y)
  (dotimes (i (min (length x) (length y)))
    (let ((c (compare (aref x i) (aref y i))))
      (unless (eq c :eq)
	(return-from compare-key c))))
  :eq)

(struct:define index _
  (root root)
  (name string :read _)
  (columns vector :init (make-array 0 :adjustable t))
  (file (or stream null))
  (records rb:tree :init (rb:new-tree :compare #'compare-key)))

(defmacro define (name &body forms)
  `(progn
     ,@(mapcar (lambda (f)
		 (ecase (kw (first f))
		   (:index
		    (let ((n (second f)))
		      (ensure-generic-function n)

		      `(define-index ,n
			 ,@(rest (rest f)))))
		   (:table
		    `(define-table ,(second f)
		       ,@(rest (rest f))))))
	       forms)

     (defun ,(sym 'new- name) (path)
       (let ((root ($root :path path)))
	 ,@(mapcar (lambda (f)
		     (ecase (kw (first f))
		       (:index
			(let ((n (second f)))
			  `(let ((idx (,(sym 'new- n) root)))
			     (defmethod ,n ((root (eql root)))
			       idx))))
		       (:table
			(let ((n (second f)))
			  `(let ((tbl (,(sym 'new- n) root)))
			     (defmethod ,n ((root (eql root)))
			       tbl))))))
		   forms)
	 root))))

(defmacro define-table (name &body forms)
  (ensure-generic-function name)

  `(progn     
     ,@(mapcar (lambda (f)
		 (ecase (kw (first f))
		   (:column)
		   (:index
		    (let ((n (second f)))
		      (ensure-generic-function (sym name '- n))
		      (ensure-generic-function n)

		      `(define-index ,(sym name '- n)
			 ,@(rest (rest f)))))))
	       forms)

     (defun ,(sym 'new- name) (root)
       (let ((tbl ($table :root root :name ,(string-downcase (symbol-name name)))))
	 (push-table tbl root)
	 ,@(mapcar (lambda (f)
		     (ecase (kw (first f))
		       (:column
			`(push-column ',(second f) tbl))
		       (:index
			(let ((n (second f)))
			  `(let ((idx (,(sym 'new- name '- n) root)))
			     (push-index idx tbl)
			     
			     (defmethod ,(sym name '- n) ((root (eql root)))
			       idx)
			     
			     (defmethod ,n ((tbl (eql tbl)))
			       idx))))))
		   forms)
	 tbl))))

(defmacro define-index (name &body forms)
  `(defun ,(sym 'new- name) (root)
       (let ((idx ($index :root root :name ,(string-downcase (symbol-name name)))))
	 (push-index idx root)
	 ,@(mapcar (lambda (c)
		     `(push-column ',c idx))
		   forms)
	 idx)))

(defmethod close-files ((root root))
  (dolist (tbl ($root-tables root))
    (close-files tbl))

  (dolist (idx ($root-indexes root))
    (close-files idx)))

(defmacro close-file (owner slot)
  `(when (,slot ,owner)
     (close (,slot ,owner))
     (setf (,slot ,owner) nil)))

(defmethod close-files ((tbl table))
  (close-file tbl $table-key-file)
  (close-file tbl $table-data-file))

(defmethod close-files ((idx index))
  (close-file idx $index-file))

(defmethod drop ((root root))
  (dolist (tbl ($root-tables root))
    (drop tbl))

  (dolist (idx ($root-indexes root))
    (drop idx)))

(defun get-path (root name ext)
  (make-pathname :directory `(:relative ,($root-path root)) :name name :type ext))

(defun table-path (tbl ext)
  (get-path ($table-root tbl) ($table-name tbl) ext))

(defmethod drop ((tbl table))
  (close-files tbl)
  (let ((p (table-path tbl "key")))
    (when (probe-file p)
      (delete-file p)))
  (let ((p (table-path tbl "dat")))
    (when (probe-file p)
      (delete-file p)))
  (clrhash ($table-records tbl)))

(defun index-path (idx)
  (get-path ($index-root idx) ($index-name idx) "idx"))

(defmethod drop ((idx index))
  (close-files idx)
  (let ((p (index-path idx)))
    (when (probe-file p)
      (delete-file p)))
  (rb:clear ($index-records idx)))

(defmethod exists? ((tbl table) id)
  (not (null (gethash id ($table-records tbl)))))

(defun field (col tbl rec)
  (let ((i (gethash col ($table-column-lookup tbl))))
    (aref rec i)))

(defun (setf field) (val col tbl rec)
  (let ((i (gethash col ($table-column-lookup tbl))))
    (setf (aref rec i) val)))

(defun find-id (id tbl)
  (let ((pos (gethash id ($table-records tbl))))
    (when pos
      (file-position ($table-data-file tbl) pos)
      (read ($table-data-file tbl) nil))))

(defun find-key (key idx)
  (rb:find-key key ($index-records idx)))

(defmethod init ((root root))
  (dolist (idx ($root-indexes root))
    (init idx))
  
  (dolist (tbl ($root-tables root))
    (init tbl)))

(defmethod init ((tbl table))
  (let ((p (table-path tbl "key")))
    (ensure-directories-exist p)
    (setf ($table-key-file tbl) (open p :direction :io :if-exists :overwrite :if-does-not-exist :create)))
  
  (setf ($table-data-file tbl)
	(open (table-path tbl "dat") :direction :io :if-exists :append :if-does-not-exist :create))
  
  (tagbody
   next
     (let ((id (read ($table-key-file tbl) nil)))
       (when id
	 (let ((pos (read ($table-key-file tbl))))
	   (setf ($table-max-id tbl) (max ($table-max-id tbl) id))
	   (sethash id ($table-records tbl) pos))
	 (go next)))))

(defmethod init ((idx index))
  (let ((p (index-path idx)))
    (ensure-directories-exist p)
    (setf ($index-file idx) (open p :direction :io :if-exists :overwrite :if-does-not-exist :create)))
  
  (tagbody
   next
     (let ((key (read ($index-file idx) nil)))
       (when key
	 (let ((id (read ($index-file idx))))
	   (if (zerop id)
	       (rb:remove-node key ($index-records idx))
	       (rb:add-node id ($index-records idx) :key key)))
	 (go next)))))

(defun new-record (tbl)
  (make-array (length ($table-columns tbl)) :initial-element nil))

(defun next-id (tbl)
  (incf ($table-max-id tbl)))

(defmethod push-column (col (tbl table))
  (setf (gethash col ($table-column-lookup tbl)) (length ($table-columns tbl)))
  (vector-push-extend col ($table-columns tbl)))

(defmethod push-column (col (idx index))
  (vector-push-extend col ($index-columns idx)))

(defmethod push-index (idx (root root))
  (push idx ($root-indexes root)))

(defmethod push-index (idx (tbl table))
  (push idx ($table-indexes tbl)))

(defun push-table (tbl root)
  (push tbl ($root-tables root)))

(defun record= (x y)
  (equalp x y))

(defmethod record-count ((tbl table))
  (hash-table-count ($table-records tbl)))

(defmethod record-count ((idx index))
  (rb:size ($index-records idx)))

(defmacro do-record (file &body body)
  `(let ((*standard-output* ,file))
     ,@body
     (terpri)
     (force-output)))

(defun index-key (rec tbl idx)
  (let* ((n (length ($index-columns idx)))
	 (k (make-array n)))
    (dotimes (i n)
      (setf (aref k i) (field (aref ($index-columns idx) i) tbl rec)))
    k))

(defun add-record (id rec tbl idx)
  (let ((key (index-key rec tbl idx)))
    (when (rb:add-node id ($index-records idx) :key key)
      (do-record ($index-file idx)
	(write key)
	(terpri)
	(write id))
      t)))

(defun remove-record (rec tbl idx)
  (let ((key (index-key rec tbl idx)))
    (when (rb:remove-node key ($index-records idx))
      (do-record ($index-file idx)
	(write key)
	(terpri)
	(write 0))
      t)))

(defun store (id rec tbl)
  (let-when prev (find-id id tbl)
    (dolist (idx ($table-indexes tbl))
      (unless (remove-record rec tbl idx)
	(error "Record missing in index ~a" ($index-name idx)))))
    
  (let ((pos (file-length ($table-data-file tbl))))
    (do-record ($table-key-file tbl)
      (write id)
      (terpri)
      (write pos))

    (file-position ($table-data-file tbl) pos)
    (do-record ($table-data-file tbl)
      (write rec))
    
    (sethash id ($table-records tbl) pos))

  (dolist (idx ($table-indexes tbl))
    (unless (add-record id rec tbl idx)
      (error "Duplicate key in index ~a" ($index-name idx)))))
