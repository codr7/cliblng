(defpackage db-tests
  (:use cl)
  (:export run))

(in-package db-tests)

(db:define db
  (table foo
	 (column bar)
	 (column baz)))

(defun run ()
  (let ((db (new-db "testdb")))
    (db:drop db)
    (db:init db)
    
    (let ((id (db:get-id (foo db)))
	  (rec '((bar . 1) (baz . 3))))
      (db:store (foo db) id rec)
      (db:close-files db)
      (db:init db)
      (assert (equal (db:find-id (foo db) id) rec)))))
