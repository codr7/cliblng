(defpackage db-tests
  (:use cl)
  (:export run))

(in-package db-tests)

(db:define db
  (index bar-index bar)
  
  (table foo
	 (column bar)
	 (column baz)
	 (index baz-index baz)))

(defun run ()
  (let ((db (new-db "testdb")))
    (db:drop db)
    (db:init db)
    
    (let ((id (db:next-id (foo db)))
	  (rec (db:new-record (foo db))))
      (setf (db:field 'bar (foo db) rec) 7
	    (db:field 'baz (foo db) rec) 14)
      (db:store id rec (foo db))
      (db:close-files db)
      (db:init db)

      (assert (= (db:record-count (foo db)) 1))
      (assert (db:record= (db:find-id id (foo db)) rec))
      
      (assert (eq (foo-baz-index db) (baz-index (foo db))))
      (assert (= (db:record-count (foo-baz-index db)) 1))
      (assert (= (db:find-key #(14) (foo-baz-index db)) id)))))
