(defpackage db
  (:use cl compare)
  (:import-from rb new-tree tree)
  (:import-from util dohash kw sethash sym)
  (:export close-files
	   define define-table drop
	   exists?
	   find-id
	   get-id
	   init
	   name
	   push-column push-index push-table
	   record-count root
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
  (columns list)
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
  (columns list)
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
  `(progn     
     (defun ,(sym 'new- name) (root)
       (let ((idx ($index :root root :name ,(string-downcase (symbol-name name)))))
	 (push-index idx root)
	 ,@(mapcar (lambda (c)
		     `(push-column ',c idx))
		   forms)
	 idx))))

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
  (gethash id ($table-records tbl)))

(defun find-id (tbl id)
  (let ((pos (gethash id ($table-records tbl))))
    (when pos
      (file-position ($table-data-file tbl) pos)
      (read ($table-data-file tbl) nil))))

(defun get-id (tbl)
  (incf ($table-max-id tbl)))

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
     (let ((rec (read ($table-key-file tbl) nil)))
       (when rec
	 (let ((id (first rec)))
	   (setf ($table-max-id tbl) (max ($table-max-id tbl) id))
	   (sethash id ($table-records tbl) (rest rec)))
	 (go next)))))

(defmethod init ((idx index))
  (let ((p (index-path idx)))
    (ensure-directories-exist p)
    (setf ($index-file idx) (open p :direction :io :if-exists :overwrite :if-does-not-exist :create)))
  
  (tagbody
   next
     (let ((rec (read ($index-file idx) nil)))
       (when rec
	 (rb:add-node ($index-records idx) (rest rec) (first rec))
	 (go next)))))

(defmethod push-column (col (tbl table))
  (push col ($table-columns tbl)))

(defmethod push-column (col (idx index))
  (push col ($index-columns idx)))

(defmethod push-index (idx (root root))
  (push idx ($root-indexes root)))

(defmethod push-index (idx (tbl table))
  (push idx ($table-indexes tbl)))

(defun push-table (tbl root)
  (push tbl ($root-tables root)))

(defmethod record-count ((tbl table))
  (hash-table-count ($table-records tbl)))

(defmethod record-count ((idx index))
  (rb:size ($index-records idx)))

(defun write-record (val file)
  (write val :stream file)
  (terpri file)
  (force-output file))

(defmethod store ((tbl table) id rec)
  (with-slots (data-file key-file record-id records) tbl
    (let ((pos (file-length ($table-data-file tbl))))
      (file-position ($table-data-file tbl) pos)
      (write-record (cons id pos) ($table-key-file tbl))
      (write-record rec ($table-data-file tbl))
      (sethash id ($table-records tbl) pos))))
