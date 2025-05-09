(in-package :zamsdeals.fdb)

(defvar *db* nil)

(defun start-client (&optional (cluster-file "/etc/foundationdb/fdb.cluster"))
  (api-version 100)
  (setq *db* (database-open cluster-file)))

(eval-when (:load-toplevel)
  (fdb:start-client))

;; normal key operations

(defun fdb-set (key value)
  (with-transaction (tr *db*)
    (setf (transaction-get tr key) value)))

(defun fdb-get (key)
  (with-transaction (tr *db*)
    (future-value (transaction-get tr key))))

(defun fdb-clear (key)
  (with-transaction (tr *db*)
    (transaction-clear tr key)))

(defun fdb-clear-range (start end)
  (with-transaction (tr *db*)
    (transaction-clear tr start end)))

(defun fdb-query-begins-with (key)
  (with-transaction (tr *db*)
    (transaction-range-query tr (range-starts-with key))))

;; tuple operations
(defun fdb-tuple-set (keylist value)
  "keylist must be a list, valuelist must be a list"
  (with-transaction (tr *db*)
    (setf (transaction-get tr (apply #'make-tuple keylist)) value)))

(defun fdb-tuple-get (keylist)
  "keylist must be a list"
  (with-transaction (tr *db*)
    (foundationdb::tuple-items (tuple-decode (future-value (transaction-get tr (apply #'make-tuple keylist)))))))

;; counters
(defun fdb-counter-set (keylist value)
  "keylist is a list, value is a number"
  (with-transaction (tr *db*)
    (counter-set (apply #'make-counter `(,*db* ,@keylist)) tr value)))

(defun fdb-counter-add (keylist value)
  (with-transaction (tr *db*)
    (counter-add (apply #'make-counter `(,*db* ,@keylist)) tr value)))

(defun fdb-counter-get (keylist)
  (with-transaction (tr *db*)
    (counter-get-transactional (apply #'make-counter `(,*db* ,@keylist)) tr)))

(defun fdb-counter-clear (keylist)
  (with-transaction (tr *db*)
    (counter-clear (apply #'make-counter `(,*db* ,@keylist)) tr)))

(defun fdb-counter-compute-sum (keylist)
  "since we use a keylist, you can find the sum at any part of that key"
  (with-transaction (tr *db*)
    (foundationdb::compute-sum (apply #'make-counter `(,*db* ,@keylist)) tr)))

(defsection @fts (:title "Full Text Search")
  """
## Introduction
Initially, I had developed FTS for the Namunswa application, to search through indexed products from Ugandan sites for price comparison. I am now going to port, and make it a simple layer atop the tuple layer, so it is universally accessible from wherever.

It is implemented using ngrams, we present a phrase to be indexed, we break it down into words, then we use these to make ngrams, then we, can use this to do BM25 calculations and then do rankings.

Full text search layer must be fully extensible to allow you so search using a key of any given length.
"""
  (inverse-document-frequency function)
  (bm25 function)
  (similarity function)
  (fts-index function)
  (fts-index-trgm function)
  (fts-index-bm25 function)
  (fts-fetch function)
  (fts-fetch-bm25 function)
  (incr-stat function)
  (get-stat function))

(defun inverse-document-frequency (df n)
  "give higher weights to rarer terms, smoothed"
  (log (+ 1.0d0 (/ (+ (- (float n) (float df)) 0.5d0)
                   (+ (float df) 0.5d0)))))

(defun bm25 (tf df l-doc l-avg &key (k1 1.5) (b 0.75) n)
  "Okapi BM25

Computes the Okapi BM25 relevance score for a term within a document.

This score quantifies how relevant a document is to a query containing the term.
It combines the term frequency (TF) within the document, the inverse document
frequency (IDF) of the term across the collection, and document length normalization.

Args:

-  tf (number): Term Frequency - The frequency of the term in the specific document.
-  df (number): Document Frequency - The number of documents in the collection
               that contain the term.
-  l-doc (number): The length of the specific document (e.g., number of words).
-  l-avg (number): The average document length across the entire collection.
-  k1 (number, optional): Parameter controlling term frequency saturation.
                         Higher values mean TF scales more linearly. Defaults to 1.5.
-  b (number, optional): Parameter controlling document length normalization (0 to 1).
                        0 means no length normalization, 1 means full normalization.
                        Defaults to 0.75.
-  n (number): The total number of documents in the collection.

Returns:
  (number): The calculated BM25 score for the term in the document.
"
  (* (inverse-document-frequency df n)
     (/ (* tf (1+ k1))
	(+ tf (* k1 (+ (1- b) (/ (* b l-doc) l-avg)))))))

(defun get-stat (&rest stats)
  (with-transaction (tr *db*)
    (counter-get-transactional (make-counter *db* (apply #'tuple-encode `("stats" ,@stats))) tr)))

(defun incr-stat (len &rest stats)
  (with-transaction (tr *db*)
    (counter-add (make-counter *db* (apply #'tuple-encode `("stats" ,@stats))) tr len)))

(defun get-total-length ()
     (with-transaction (tr *db*)
       (counter-get-transactional (make-counter *db* (tuple-encode "fts" "total-length")) tr)))

(defun get-number-of-docs ()
  (with-transaction (tr *db*)
    (counter-get-transactional (make-counter *db* (tuple-encode "fts" "number-of-docs")) tr)))

(defun fts-index (index text &key key (ngram-size 3) (type :trgm))
  (case type
    (:trgm (fts-index-trgm index text :key key))
    (:bm25 (fts-index-bm25 index text :key key :ngram-size ngram-size))))

(defun fts-index-bm25 (index text &key key (ngram-size 3))
  "Create an ngram index from the given text with given key, when text = \"\", we are deleting the index entry

- break the data to index into ngrams, starting from 1-grams, then save these

** Sharded counter
We have a challenge with using shared keys for total-length, which has the length of entries in the index and number-of-documents which has the number of documents in the index. It being a single key, will be read by clients in a concurrent setup, this will lead to retries and blocks, to solve that we use shards, a single key is divided into 16 pieces which will be choosen at random. this gives a good number to prevent delays in the clients. Similar behaviour must be implemented in the fetch function.

** Concurrent access
Two stat items are used across the entire index, total-length and number-of-docs. This creates a bottleneck for the concurrent access as transactions will lock down both, since with foundationdb we excell via concurrent access, we can't have this.

So we will use a distributed counter. Each index with have a number of shards. A shard is a an element in a array for a given variable, the value is the sum of all members in the array, we will use 16.

Given there are two variables, each will have its own random number to prevent collision even further, because, at scale, even the smallest possibilities become visible.
 
** Tips on speed
This function is generally slow but can be sped up by:

- Use a larger `ngram-size`, trigrams are good.
- Use concurrent clients, the clients will run in almost the same time for the same operations, if you are doing 1000 operations, a single client might take 25 seconds, while 4 clients doing 4 operations each will take 50 seconds each. Foundationdb is very good at concurrent and parallel operations.
- Memory engine is slightly faster than SSD, wasn't not much faster in benchmarks."
  ;; we try to put as much work as possible outside the transaction to save on both time and size of it.
  ;; because of sharing the general stats between documents, like total-length and number of documents, this limits the performace is a
  ;; concurrent environment, i wonder, how can we make this faster. because it is clearly a bottleneck.
  (let* ((ngrams (generate-substring-counts (tokenize text) ngram-size))
	 (text-length (length text))
	 (text-bytes (store nil text))
	 (1-octets #(1 0 0 0 0 0 0 0))
	 (-1-octets #(255 255 255 255 255 255 255 255))
	 (total-length-key (tuple-encode "fts" "bm25" "stats" "total-length" index (format nil "~a" (random 16))))
	 (number-of-docs-key (tuple-encode "fts" "bm25" "stats" "number-of-docs" index (format nil "~a" (random 16))))
	 (ngram-range-start-key (apply #'tuple-encode `("fts" "bm25" "ngrams" ,index "" ,@key)))
	 (ngram-range-stop-key (apply #'tuple-encode `("fts" "bm25" "ngrams" ,index #xFF ,@key))))
    (with-transaction (tr *db*)
      ;; first get the data to be used for updates, the length and bytes
      (let* ((saved-bytes (future-value (transaction-get tr (apply #'tuple-encode `("fts" "bm25" "data" ,index ,@key)))))
	     (saved-length-octets (future-value (transaction-get tr (apply #'tuple-encode `("fts" "bm25" "stats" "doc-length",index ,@key)))))
	     (saved-length (if (null saved-length-octets) 0 (octets->int64 saved-length-octets)))
	     ;; localise the text length to a universal one accounting for the saved one
	     ;; subtract the old from new length
	     (l-doc-octets (int64->octets (- text-length saved-length))))
	;; next, if saved, clear the ngrams of this key also, decrement the dfs by one for each saved ngram
	(when saved-bytes
	  (transaction-clear tr ngram-range-start-key ngram-range-stop-key)
	  ;; this operation is probably inefficient, will optimise later.
	  (let ((saved-ngrams (mapcar #'car (generate-substring-counts (tokenize (restore saved-bytes)) ngram-size))))
	    ;; decrement saved ngrams' df's
	    ;; we tried using the counters of cl-foundationdb but those were very slow,
	    ;; so we have resorted to using atomic operations, not that these use little-endian int64 byte arrays
	    ;; we employ cl-intbytes to do the job, we will replace all counters with this.
	    ;; but we already know the value of -1 and 1 so we will use directly.
	    (dolist (ngram saved-ngrams)
	      (foundationdb::transaction-atomic-operate tr (tuple-encode "fts" "bm25" "stats" "df" index ngram) -1-octets :add))))
	;; then store the bytes
	(setf (transaction-get tr (apply #'tuple-encode `("fts" "bm25" "data" ,index ,@key))) text-bytes)
	;; save ngrams
	(dolist (ngram-data ngrams)
	  (let ((ngram (car ngram-data)))
	    (foundationdb::transaction-atomic-operate tr (tuple-encode "fts" "bm25" "stats" "df" index ngram) 1-octets :add)
	    (setf (transaction-get tr (apply #'tuple-encode `("fts" "bm25" "ngrams" ,index ,ngram ,@key)))
		  (store nil (cdr ngram-data)))))
	;; then update the stats used for bm25 computation
	(unless (equalp text-bytes saved-bytes)
	  ;; if both are equal, no need to run this code, because we will be updating stats wrongly, nothing is changing
	  (foundationdb::transaction-atomic-operate tr (apply #'tuple-encode `("fts" "bm25" "stats" "doc-length" ,index ,@key)) l-doc-octets :add)
	  ;; update the total-length with l-doc
	  ;; increase concurrency by randomising the total and number of docs here
	  (foundationdb::transaction-atomic-operate tr total-length-key l-doc-octets :add)
	  ;; update the number of docs when there's no old doc
	  (cond ((null saved-bytes)
		 (foundationdb::transaction-atomic-operate tr number-of-docs-key 1-octets :add))
		((string= text "") ;; remove the doc if text is ""
		 (foundationdb::transaction-atomic-operate tr number-of-docs-key -1-octets :add))))))))

(defun fts-fetch (index phrase &key (ngram-size 3) (type :trgm))
  (case type
    (:trgm (fts-fetch-trgm index phrase :key key :ngram-size ngram-size))
    (:bm25 (fts-fetch-bm25 index phrase :key key :ngram-size ngram-size))))

(defun fts-fetch-bm25 (index phrase &key key (ngram-size 3))
  "fetch fts items"
  (let* ((ngrams (mapcar #'car (generate-substring-counts (tokenize phrase) ngram-size)))
	 (bm25 (hash))
	 (df (hash))
	 (tf-hash (hash))
	 (l-doc-hash (hash))
         number-of-all-docs l-avg intersection)
    ;; fetch dfs within a single transaction
    (with-transaction (tr *db*)
      (setf number-of-all-docs
	    (reduce #'+ (mapcar (lambda (i) (octets->int64 (cadr i)))
				(transaction-range-query tr (tuple-encode "fts" "bm25" "stats" "number-of-docs" index "0")
							 (tuple-encode "fts" "bm25" "stats" "number-of-docs" index "15")))))
      
      (setf l-avg (/
		   (reduce #'+ (mapcar (lambda (i) (octets->int64 (cadr i)))
				       (transaction-range-query tr (tuple-encode "fts" "bm25" "stats" "total-length" index "0")
								(tuple-encode "fts" "bm25" "stats" "total-length" index "15"))))
		   number-of-all-docs))
      (dolist (ngram ngrams)
	(setf (gethash ngram df 0)
	      (octets->int64 (future-value (transaction-get tr (tuple-encode "fts" "bm25" "stats" "df" index ngram)))))))
    ;; fetch matching ngrams
    ;; limit the search
    (with-transaction (tr *db*)
      (dolist (ngram ngrams)
	;; when fetching the ngrams, what we are doing is fetching all ngrams with a given key,
	;; so the limits must be applied with the key (#xFF), there's no way to do that in here
	;; without getting unwmated elements, which means this will be a range starts with
	;; this part collects the tfs
	(let* ((data (transaction-range-query tr (range-starts-with (apply #'tuple-encode `("fts" "bm25" "ngrams" ,index ,ngram ,@key))))))
	  (setf intersection (union intersection data :test #'equalp :key #'car)))))
    ;; this part collects the l-docs for given keys and also the tfs with (ngram . key)
    (with-transaction (tr *db*)
      (loop for (key-1 tf) in intersection
	    do
	       (let* ((key-data (foundationdb::tuple-items (tuple-decode key-1)))
		      (key1 (subseq (coerce key-data 'list) 3))
		      (tf-key (cons (aref key-data 2) key1)))
		 ;; tf cache
		 (or (gethash tf-key tf-hash)
		     (setf (gethash tf-key tf-hash) (restore tf)))
		 ;; cache l-doc
		 (or (gethash key1 l-doc-hash)
		     (setf (gethash key1 l-doc-hash)
			   (octets->int64 (future-value (transaction-get tr (apply #'tuple-encode `("fts" "bm25" "stats"  "doc-length" ,index ,@key1))))))))))
    (print tf-hash)
    ;; isolate the bm25 calculation from the query loop.
    (loop for (tf-key . tf) in (hash->alist tf-hash)
	  do (let* ((key1 (cdr tf-key))
		    (ngram (car tf-key)))
	       (incf (gethash key1 bm25 0)
		     (bm25 tf (gethash ngram df) (gethash key1 l-doc-hash) l-avg :n number-of-all-docs))))
    (let* ((alist (zsort:quicksort (hash->alist bm25) #'> :key #'cdr))
	   (alist-len (length alist)))
      (cons alist-len (with-transaction (tr *db*)
			(loop for (kv . bm25) in alist
			      collect (let ((data (future-value (transaction-get tr (apply #'tuple-encode `("fts" "bm25" "data" ,index ,@kv))))))
					(cons kv (restore data)))))))))

(defun similarity (ngrams-1 ngrams-2 shared-ngrams)
  "compute the similarity between two texts using their ngrams"
  (/ shared-ngrams
     (+ ngrams-1 (- ngrams-2 shared-ngrams))))

(defun make-trigrams (str)
  (let (lst)
    (dolist (token (tokenize str)) (setf lst `(,@lst ,@(make-trigrams-1 token))))
    (remove-duplicates lst :test #'string= :from-end nil)))

(defun make-trigrams-1 (str)
  (let ((len (length str))
        (generated-tokens '()))
    ;; (when (>= len 1)
    ;;   (push (subseq str (- len 1) len) generated-tokens))
    (when (>= len 2)
      (push (subseq str (- len 2) len) generated-tokens))
    (loop for i from (- len 3) downto 0
          do (push (subseq str i (+ i 3)) generated-tokens))
    (when (>= len 2)
      (push (subseq str 0 2) generated-tokens))
    (when (>= len 1)
      (push (subseq str 0 1) generated-tokens))
    (remove-duplicates (nreverse generated-tokens) :test #'string= :from-end nil)))

(defun fts-index-trgm (index text &key key)
  "Create a trigram index for a given text"
  (let* ((ngrams (mapcar #'car (generate-substring-counts (tokenize text) 3)))
	 (number-of-ngrams (store nil (length ngrams)))
	 (text-octets (store nil text)))
    (with-transaction (tr *db*)
      (setf (transaction-get tr (apply #'tuple-encode `("fts" "trgm" "length" ,index ,@key))) number-of-ngrams)
      (setf (transaction-get tr (apply #'tuple-encode `("fts" "trgm" "data" ,index ,@key))) text-octets)
      (dolist (ngram ngrams)
	(setf (transaction-get tr (apply #'tuple-encode `("fts" "trgm" "ngrams" ,index ,ngram ,@key))) "")))))

(defun fts-fetch-trgm (index phrase &key (similarity-threshold 0.3))
  "fetch fts items"
  (let* ((ngrams (mapcar #'car (generate-substring-counts (tokenize phrase) 3)))
	 (ngrams-2 (length ngrams))
	 (shared-ngrams (hash))
	 (similarity (hash))
	 (doc-lengths (hash)))
    ;; collect ngrams
    (dolist (ngram ngrams)
      (with-transaction (tr *db*)
	(do-range-query ((key value) tr (range-starts-with (apply #'tuple-encode `("fts" "trgm" "ngrams" ,index ,ngram))))
	  (declare (ignore value))
	  (let* ((key-data (foundationdb::tuple-items (tuple-decode key)))
		 (key1 (subseq (coerce key-data 'list) 5)))
	    (incf (gethash key1 shared-ngrams 0))))))
    (print shared-ngrams)
    ;; get doc-lengths
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (with-transaction (tr *db*)
		 (setf (gethash k doc-lengths) (future-value (transaction-get tr (apply #'tuple-encode `("fts" "trgm" "length" ,index ,@k)))))))
	     shared-ngrams)
    (print doc-lengths)
    ;; get similarities
    (maphash (lambda (k v)
	       (setf (gethash k similarity) (similarity (restore (gethash k doc-lengths))
							ngrams-2
	       						v)))
	     shared-ngrams)
    (print similarity)
    ;; remove all with similarity < 0.3
    (loop for (k . v) in (remove-if (lambda (i) (< i similarity-threshold)) (hash->alist similarity) :key #'cdr)
	  collect (with-transaction (tr *db*)
		    (cons k (restore (future-value (transaction-get tr (apply #'tuple-encode `("fts" "trgm" "data" ,index ,@k))))))))))

(defun test-fts-index-parrallel ()
  (mapcar #'sb-thread:join-thread
	  (loop for i from 0 below 4
		collect (let ((*db* *db*)
			      (i i))
			  (sb-thread:make-thread (lambda () (test-fts-index-loop (* i 25000) (+ 25000 (* i 25000)))))))))

(defun test-fts-index-loop (&optional (start 0) (stop 1000))
  (loop for i from start to stop
	do (test-fts-index i)))

(defun test-fts-index (i)
  (fts-index "test-trgm" (format nil "document: ~a, when fetching the ngrams, what we are doing is fetching all ngrams with a given key,
	so the limits must be applied with the key (#xFF), there's no way to do that in here
	without getting unwmated elements, which means this will be a range starts with
	this part collects the tfs" i) :type :bm25 :key (list (format nil "~a" i)) :ngram-size 3))
