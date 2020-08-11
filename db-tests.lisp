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
      (setf (db:field 'bar (foo db) rec) 1
	    (db:field 'baz (foo db) rec) 3)
      (db:store (foo db) id rec)
      (db:close-files db)
      (db:init db)

      (assert (= (db:record-count (foo db)) 1))
      (assert (db:record= (db:find-id (foo db) id) rec))
      
      (assert (= (db:record-count (bar-index db)) 1))

      (assert (eq (foo-baz-index db) (baz-index (foo db))))
      (assert (= (db:record-count (foo-baz-index db)) 1)))))
