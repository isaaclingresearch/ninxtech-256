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

(handler-case (change-toplevel-database "namunswa" "namunswa" (uiop:getenv "POSTGRES_PASSWORD") "localhost")
  (error (err)
    (format t "Error connecting to database, with the error: ~a.~% If database is not created, create it." err)))

(defmacro conn ((database) &body data)
  `(with-connection (list ,database ,database ,(uiop:getenv "POSTGRES_PASSWORD") "localhost")
     ,@data))

(defun make-date-string ()
  "return date as YYYY-MM-DD"
  (car (str:split "T" (format nil "~a" (local-time:today)))))

(test start-tests (is (equalp "postgres" (change-toplevel-database "postgres" "postgres" (uiop:getenv "POSTGRES_PASSWORD") "localhost"))))
(test delete-db (is (null (query (:drop-database "namunswa_testdb")))))
(test drop-role (is (null (query (:drop-role "namunswa_testdb")))))

(defun initialise-db (&key (role "namunswa") (database "namunswa") (create-tables t))
  "this function will create the database and the appropriate tables."
  (let ((password (uiop:getenv "POSTGRES_PASSWORD")))
    (change-toplevel-database "postgres" "postgres" password "localhost")
    (create-role role password :base-role :admin)
    (create-database database :owner role)
    (when create-tables
      (change-toplevel-database role database password "localhost")
      (create-tables))))

(defparameter *db-string* "namunswa" "This is the database name currently in use, we need this to reduce code and make tests work.")
(test initialise-db (is (null (initialise-db :role "namunswa_testdb" :database "namunswa_testdb" :create-tables t))))
(test set-test-db (not (null (setf *db-string* "namunswa_testdb"))))

(defun create-tables ()
  "this function will create tables for storing the data, the user-uuid is the main identifier of the user."
  #|
  since we might not be able to get timestamps and we need to sort items by recency, we will use a crawl id and a timestamp for addition, we will use these in sorting to get the most recent crawls and the most recent items intracrawl ;
  |#
  (load-extension "pg_trgm") ;; for autocomplete search
  (conn (*db-string*)

	;; analytics tables, use a single table, having the event type, count, date
	(query (:create-table (:if-not-exists 'analytics)
			      ((event-type :type varchar)
			       (page :type varchar)
			       (count :type integer :default 1)
			       (date :type timestamp-without-time-zone))
			      (:primary-key event-type page date)))
	
	(query (:create-table (:if-not-exists 'crawls)
			      ((id :type serial :primary-key t)
			       (creation-date :type timestamp-without-time-zone :default (:raw "CURRENT_TIMESTAMP")))))
	(query
	 (:create-table (:if-not-exists 'item-table)
			((id :type uuid :primary-key t :default (:raw "gen_random_uuid()"))
			 (crawl-id :type integer :references ((crawls id)))
			 (title :type (or varchar db-null))
			 (site :type (or varchar db-null))
			 (condition :type (or varchar db-null))
			 (cost :type integer)
			 (region :type (or varchar db-null))
			 (status :type (or varchar db-null))
			 (external-id :type varchar)
			 (short-description :type (or varchar db-null))
			 (details :type (or varchar db-null))
			 (full-text-vector :type tsvector)
			 (images :type (array text))
			 (product-url :type varchar)
			 (user-phone :type (or varchar db-null))
			 (external-date :type (or db-null timestamp-without-time-zone))
			 (creation-date :type timestamp-without-time-zone :default (:raw "CURRENT_TIMESTAMP")))
			(:constraint unique_external_id_and_site :unique 'site 'external-id)))

	;; create an index for the full text search on items table
	(query (:create-index 'item-full-text-vector-idx :on 'item-table :using 'gin :fields 'full-text-vector))
	;; create a trigram index for autocomplete searc
	(query (:create-index 'item-title-trigram-idx :on 'item-table :using 'gin :fields (:gin-trgm-ops 'title)))
	(query (:create-index 'item-short-desc-trigram-idx :on 'item-table :using 'gin :fields (:gin-trgm-ops 'short-description)))
	(query (:create-index 'item-details-trigram-idx :on 'item-table :using 'gin :fields (:gin-trgm-ops 'details)))

	))
(test create-tables (is (null (create-tables))))

(defun delete-tables ()
  "delete all tables"
  (format t "~%~% deleting tables: ~a~%~%" *db-string*)
  (conn (*db-string*)
	(dolist (table '(item-table crawls analytics))
	  (query (:drop-table table)))))
(test delete-tables (is (null (delete-tables))))

(defun reset-tables ()
  (handler-case (delete-tables) (error (err) (declare (ignore err))))
  (create-tables))
(test reset-tables (is (null (reset-tables))))

(defun set-crawl ()
  "save and return a new crawl-id"
  (conn (*db-string*)
	(query (:insert-into 'crawls :set :returning 'id) :single)))
(test set-crawl (is (equal 1 (set-crawl))))

(defun save-item (crawl-id data &optional (site "jiji"))
  (let ((full-text (format nil "~{~a ~}" (list
					  (gethash "title" data)
					  site
					  (gethash "condition" data)
					  (gethash "cost" data)
					  (gethash "region" data)
					  (gethash "status" data)
					  (gethash "short-description" data)
					  (gethash "details" data)))))
    (conn (*db-string*)
	  (query (:insert-into 'item-table
		  :set 'crawl-id crawl-id
		  'title (gethash "title" data) 'site site 'condition (gethash "condition" data)
		  'cost (gethash "cost" data) 'region (gethash "region" data) 'status (gethash "status" data)
		  'details (gethash "details" data) 'external-id (gethash "external-id" data)
		  'product-url (gethash "product-url" data)
		  'short-description (gethash "short-description" data) 'user-phone (gethash "user-phone" data)
		  'images (gethash "images" data) 'full-text-vector (:to-tsvector "english" full-text)
		  :on-conflict-do-nothing)))))
(test save-item (is (null (save-item 1 (hash ("title" "title")
					     ("condition" "condition")
					     ("cost" 1234)
					     ("region" "central")
					     ("status" "Active")
					     ("details" "details")
					     ("external-id" "id")
					     ("short-description" "test description")
					     ("user-phone" "08953")
					     ("images" #("a" "b" "c")))))))

(defun search-items (phrase page-number filter)
  (trivia:match filter
    ("relevance" (search-items-by-latest phrase page-number))
    ("price-low-to-high" (search-items-by-price-low-to-high phrase page-number))
    ("price-high-to-low" (search-items-by-price-high-to-low phrase page-number))))

(defun search-items-by-latest (phrase page-number)
  (conn (*db-string*)
	(query (:fetch
		(:order-by 
		 (:select '* :from 'item-table :where (:|@@| 'full-text-vector (:plainto-tsquery "english" phrase)))
		 (:desc'crawl-id) (:desc 'creation-date))
		10 (* 10 (1- page-number)))
	       :plists)))
(test search-jiji-items-by-latest (is (equal 1 (length (search-items-by-latest "condition Active details" 1)))))

(defun search-items-by-price-low-to-high (phrase page-number)
  (conn (*db-string*)
	(query (:fetch
		(:order-by 
		 (:select '* :from 'item-table :where (:|@@| 'full-text-vector (:plainto-tsquery "english" phrase)))
		 (:asc 'cost)
		 (:desc'crawl-id) (:desc 'creation-date))
		10 (* 10 (1- page-number)))
	       :plists)))
(test search-jiji-items-by-price-low-to-high (is (equal 1 (length (search-items-by-price-low-to-high "condition Active details" 1)))))

(defun search-items-by-price-high-to-low (phrase page-number)
  (conn (*db-string*)
	(query (:fetch
		(:order-by 
		 (:select '* :from 'item-table :where (:|@@| 'full-text-vector (:plainto-tsquery "english" phrase)))
		 (:desc 'cost)
		 (:desc'crawl-id) (:desc 'creation-date))
		10 (* 10 (1- page-number)))
	       :plists)))
(test search-jiji-items-by-price-high-to-low (is (equal 1 (length (search-items-by-price-high-to-low "condition Active details" 1)))))

(defun count-items (phrase)
  (conn (*db-string*)
	(query (:select (:count '*) :from 'item-table :where (:|@@| 'full-text-vector (:plainto-tsquery "english" phrase)))
	       :single)))
(test count-items (is (equal 1 (count-items "condition details"))))

(defun get-autocomplete-terms (phrase)
  (conn (*db-string*)
	(query (:fetch (:order-by (:select 'title :from 'item-table
				   :where (:or (:ilike 'title (format nil "%~a%" phrase))
					       (:% 'short-description (format nil "%~a%" phrase))
					       (:% 'details (format nil "%~a%" phrase)))
				   )
				  (:desc (:+ (:similarity 'title phrase) (:similarity 'short-description phrase) (:similarity 'details phrase)))
				  (:desc 'crawl-id) (:desc 'creation-date))
		       10))))
(test get-autocomplete-terms (is (equal 1 (length (get-autocomplete-terms "details")))))

;; analytics
(defun incr-analytics (page event-type &optional (date (make-date-string)))
  (conn (*db-string*)
	(query (:insert-into 'analytics :set 'page page 'event-type event-type 'date date
			     :on-conflict 'date 'page 'event-type
			     :update-set 'count (:+ 1 'analytics.count)))))
(test incr-analytics (is (null (incr-analytics "page" "view"))))

(defun get-day-analytics (page event-type date)
  "get the data for event and page, return a cons list (date . count)"
  (conn (*db-string*)
	(trivia:match (query (:select 'count :from 'analytics
			      :where (:and (:= 'page page) (:= 'event-type event-type)
					   (:= 'date date)))
			     :single)
	  (nil 0)
	  (:null 0)
	  (else else))))
(test get-day-analytics (is (equal 1 (get-day-analytics "page" "view" (make-date-string)))))

(defun get-analytics (page event-type &optional (days 1))
  "get the data for event and page, return a cons list (date . count)"
  (conn (*db-string*)
	(query (:order-by (:select (:as (:raw (format nil "to_char(date, 'YYYY-MM-DD')")) 'date) 'count
				   :from 'analytics
				   :where (:and (:= 'page page) (:= 'event-type event-type)
						(:> 'date (:raw (format nil "(DATE '~a' - INTERVAL '~a days')" (make-date-string) days)))))
			  'date)
	       :plists)))
(test get-analytics (is (equal 1 (length (get-analytics "page" "view")))))

(defun get-summed-analytics (page event-type &optional (days 1))
  "return a number for the number of events on a given page in a given number of days from today"
  (trivia:match (conn (*db-string*)
		      (query (:select (:sum 'count)
			      :from 'analytics
			      :where (:and (:= 'page page) (:= 'event-type event-type)
					   (:> 'date (:raw (format nil "(DATE '~a' - INTERVAL '~a days')" (make-date-string) days))))) :single))
    (:null 0)
    (else else)))
(test get-summed-analytics (is (equal 1 (get-summed-analytics "page" "view"))))

;; last test returns to namunswa db
(test return-to-namunswa-db (is (equal "namunswa"
				       (progn
					 (reset-tables)
					 (change-toplevel-database "namunswa" "namunswa" (uiop:getenv "POSTGRES_PASSWORD") "localhost")
					 (setf *db-string* "namunswa")))))
