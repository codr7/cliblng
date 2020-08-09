(defpackage db
  (:use cl)
  (:import-from util dohash kw sethash sym)
  (:export close-files column
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

(struct:define column _
  (name symbol))

(struct:define table _
  (root root)
  (name string :read _)
  (columns list)
  (key-file (or stream null))
  (data-file (or stream null))
  (max-id integer :init 0)
  (records hash-table :init (make-hash-table :test 'eq)))

(defmacro define (name &body forms)
  `(progn
     ,@(mapcar (lambda (f)
		 (ecase (kw (first f))
		   (:table
		    (let ((name (second f))
			  (forms (rest (rest f))))
		      `(define-table ,name
			 ,@forms)))))
	       forms)

     (struct:define ,name root
       ,@(mapcar (lambda (f)
		   (ecase (kw (first f))
		     (:table
		      `(,(second f) (or table null) :init nil :read _))))
		 forms))

     (defun ,(sym 'new- name) (path)
       (let ((root (,(sym '$ name) :path path)))
	 ,@(mapcar (lambda (f)
		     (ecase (kw (first f))
		       (:table
			(let ((tn (second f)))
			  `(setf (,(sym '$ name '- tn) root) (,(sym 'new- tn) root))))))
		   forms)
	 root))))

(defmacro define-table (name &body forms)
  `(progn
     ,@(mapcar (lambda (f)
		 (ecase (kw (first f))
		   (:column
		    (let ((cn (second f)))
		      `(defparameter ,(sym name '- cn) ($column :name ',cn))))))
	       forms)

     (defun ,(sym 'new- name) (root)
       (let ((tbl ($table :root root :name ,(string-downcase (symbol-name name)))))
	 (push-table root tbl)
	 ,@(mapcar (lambda (f)
		     (ecase (kw (first f))
		       (:column
			`(push-column tbl ,(sym name '- (second f))))))
		   forms)
	 tbl))))

(defmethod close-files ((root root))
  (dolist (tbl ($root-tables root))
    (close-files tbl))

  (dolist (idx ($root-indexes root))
    (close-files idx)))

(defmacro close-file (tbl slot)
  `(when (,slot ,tbl)
     (close (,slot ,tbl))
     (setf (,slot ,tbl) nil)))

(defmethod close-files ((tbl table))
  (close-file tbl $table-key-file)
  (close-file tbl $table-data-file))

(defmethod drop ((root root))
  (dolist (tbl ($root-tables root))
    (drop tbl))

  (dolist (idx ($root-indexes root))
    (drop idx)))

(defmethod drop ((tbl table))
  (close-files tbl)
  (let ((p (key-path tbl)))
    (when (probe-file p)
      (delete-file p)))
  (let ((p (data-path tbl)))
    (when (probe-file p)
      (delete-file p)))
  (clrhash ($table-records tbl)))

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

(defun table-path (tbl ext)
  (make-pathname :directory `(:relative ,($root-path ($table-root tbl))) :name ($table-name tbl) :type ext))

(defun key-path (tbl)
  (table-path tbl "key"))

(defun data-path (tbl)
  (table-path tbl "dat"))

(defmethod init ((tbl table))
  (ensure-directories-exist (key-path tbl))
  (setf ($table-key-file tbl) (open (key-path tbl) :direction :io :if-exists :overwrite :if-does-not-exist :create))
  (setf ($table-data-file tbl) (open (data-path tbl) :direction :io :if-exists :append :if-does-not-exist :create))
  
  (tagbody
   next
     (let ((rec (read ($table-key-file tbl) nil)))
       (when rec
	 (let ((id (first rec)))
	   (setf ($table-max-id tbl) (max ($table-max-id tbl) id))
	   (sethash id ($table-records tbl) (rest rec)))
	 (go next)))))

(defun push-column (tbl col)
  (push col ($table-columns tbl)))

(defun push-index (root idx)
  (push idx ($root-indexes root)))

(defun push-table (root tbl)
  (push tbl ($root-tables root)))

(defmethod record-count ((tbl table))
  (hash-table-count ($table-records tbl)))

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
