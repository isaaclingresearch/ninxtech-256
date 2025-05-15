(in-package :s-sql)

(def-sql-op :plainto-tsquery (&rest args)
  `("plainto_tsquery (" ,@(sql-expand-list args) ")"))

(def-sql-op :similarity (&rest args)
  `("similarity (" ,@(sql-expand-list args) ")"))

(in-package :256.db)

;; connect to decklm database.
(handler-case (connect-toplevel "postgres" "postgres" (uiop:getenv "POSTGRES_PASSWORD") "localhost")
  (error (err)
    (format t "Error connecting to database, with the error: ~a.~% If database is not created, create it." err)))

(handler-case (change-toplevel-database "db_256" "user_256" (uiop:getenv "POSTGRES_PASSWORD") "localhost")
  (error (err)
    (format t "Error connecting to database, with the error: ~a.~% If database is not created, create it." err)))

(defmacro conn ((database) &body data)
  `(with-connection (list ,database ,database ,(uiop:getenv "POSTGRES_PASSWORD") "localhost")
     ,@data))

(defun make-date-string ()
  "return date as YYYY-MM-DD"
  (car (str:split "T" (format nil "~a" (local-time:today)))))

(test start-tests (is (equalp "postgres" (change-toplevel-database "postgres" "postgres" (uiop:getenv "POSTGRES_PASSWORD") "localhost"))))
(test delete-db (is (null (query (:drop-database "testdb_256")))))
(test drop-role (is (null (query (:drop-role "testdb_256")))))

(defun initialise-db (&key (role "user_256") (database "db_256") (create-tables t))
  "this function will create the database and the appropriate tables."
  (let ((password (uiop:getenv "POSTGRES_PASSWORD")))
    (change-toplevel-database "postgres" "postgres" password "localhost")
    (create-role role password :base-role :admin)
    (create-database database :owner role)
    (when create-tables
      (change-toplevel-database database role password "localhost")
      (create-tables))))

(defparameter *db-string* "db_256" "This is the database name currently in use, we need this to reduce code and make tests work.")
(test initialise-db (is (null (initialise-db :role "testdb_256" :database "testdb_256" :create-tables t))))
(test set-test-db (not (null (setf *db-string* "testdb_256"))))

(defun create-tables ()
  "We start with measurements. we will need a lot of data if we are to improve the site, that means we must log everything that happens with the users:

** Visits:

1. Session.
2. IP: Pages visited, time spent per page"
  (load-extension "pg_trgm") ;; for autocomplete search
  (conn (*db-string*)
	;; holds the referring pages per ip/user.
	(query (:create-table (:if-not-exists 'previsit-page)
			      ((user-id :type (or varchar db-null))
			       (ip :type varchar)
			       (page :type (or varchar db-null))
			       (date :type timestamp-without-time-zone :default (:raw "CURRENT_TIMESTAMP")))
			      (:primary-key ip date)))))
(test create-tables (is (null (create-tables))))

(defun delete-tables ()
  "delete all tables"
  (format t "~%~% deleting tables: ~a~%~%" *db-string*)
  (conn (*db-string*)
	(dolist (table '(pre-visit-page))
	  (query (:drop-table table)))))
(test delete-tables (is (null (delete-tables))))

(defun set-previsit-page (ip page)
  (conn (*db-string*) (query (:insert-into 'previsit-page :set 'ip ip 'page page))))

(defun reset-tables ()
  (handler-case (delete-tables) (error (err) (declare (ignore err))))
  (create-tables))
(test reset-tables (is (null (reset-tables))))

;; last test returns to 256 db
(test return-to-256-db (is (equal "db_256"
				       (progn
					 (reset-tables)
					 (change-toplevel-database "db_256" "user_256" (uiop:getenv "POSTGRES_PASSWORD") "localhost")
					 (setf *db-string* "db_256")))))
