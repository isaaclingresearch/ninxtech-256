(in-package :zamsdeals.fdb)

(defvar *db* nil)

(defun start-client (&optional (cluster-file "/etc/foundationdb/fdb.cluster"))
  (api-version 100)
  (setq *db* (database-open cluster-file)))

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
  (fts-index function)
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

(defun fts-index (text &rest key)
  "Given a value and `&rest` for key values:

- break the data to index into ngrams, starting from 1-grams, then save these"
  (let* ((ngrams (generate-substring-counts (tokenize text) 1))
	 (text-length (length text))
	 (text-bytes (store nil text))
	 (saved-length (apply #'get-stat `("fts" "doc-length" ,@key)))
	 (saved-bytes (with-transaction (tr *db*)
			(future-value (transaction-get tr (apply #'tuple-encode `("fts" "data" ,@key)))))))
      (with-transaction (tr *db*)
	(when saved-bytes
	  (transaction-clear tr (apply #'tuple-encode `("fts" "ngrams" "" ,@key))
			     (tuple-encode "fts" "ngrams" #xFF ,@key))
	  (let ((saved-ngrams (mapcar #'car (generate-substring-counts (restore saved-bytes)))))
	    ;; decrement saved ngrams' df's
	    (dolist (ngram saved-ngrams)
	      (counter-add (make-counter *db* (tuple-encode "fts" "df" ngram) tr -1)))))
	(setf (transaction-get tr (apply #'tuple-encode `("fts" "data" ,@key))) text-bytes)
		  ;; save ngrams
	(dolist (ngram-data ngrams)
	  (let ((ngram (car ngram-data)))
	    (counter-add (make-counter *db* (tuple-encode "fts"  "df" ngram)) tr 1)
	    (setf (transaction-get tr (apply #'tuple-encode `("fts" "ngrams" ,ngram ,@key)))
		  (store nil (cdr ngram-data)))))
	(unless (equalp text-bytes saved-bytes)
	  (if saved-bytes
	      (apply #'incr-stat `(,(- text-length (if (null saved-length) 0 saved-length)) "fts"  "doc-length" ,@key))
	      (apply #'incr-stat `(,text-length "fts"  "doc-length" ,@key))) ;; for search (bm25)
	 (apply #'incr-stat `(,(- text-length (if (null saved-length) 0 saved-length)) "fts"  "total-length"))
	 (when (null saved-bytes)
	   (counter-add (make-counter *db* (tuple-encode "fts"  "number-of-docs")) tr 1)) ;; for search (bm25)
	  ))))

(defun fts-fetch (phrase &key (page-number 1) key (ngram-size 3))
  "fetch fts items"
  (let* ((ngrams (mapcar #'car (generate-substring-counts (tokenize phrase) ngram-size)))
	 (bm25 (hash))
	 (df (hash))
	 (tf-hash (hash))
	 (l-doc-hash (hash))
         (number-of-all-docs (get-stat "fts"  "number-of-docs"))
	 (l-avg (/ (apply #'get-stat `("fts"  "total-length")) number-of-all-docs))
	 intersection)
    ;; fetch dfs within a single transaction
    (with-transaction (tr *db*)
      (dolist (ngram ngrams)
	(setf (gethash ngram df 0)
	      (counter-get-transactional (make-counter *db* (tuple-encode "fts"  "df" ngram)) tr))))
    ;; fetch matching ngrams
    ;; limit the search
    (with-transaction (tr *db*)
      (dolist (ngram ngrams)
	(let* ((data (transaction-range-query tr
					      (apply #'tuple-encode `("fts" "ngrams" ,ngram
									    ,@(mapcar (lambda (i) "") key)))
					      (apply #'tuple-encode `("fts" "ngrams" ,ngram
									    ,@(mapcar (lambda (i) #xFF) key)))
					      :limit 1000)))
	  (setf intersection (union intersection data :test #'equalp :key #'car)))))
    
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
			   (counter-get-transactional (make-counter *db* (apply #'tuple-encode `("fts"  "doc-length" ,@key1))) tr))))))
    ;; isolate the bm25 calculation from the query loop.
    (loop for (tf-key . tf) in (hash->alist tf-hash)
	  do (let* ((key1 (cdr tf-key))
		    (ngram (car tf-key)))
	       (incf (gethash key1 bm25 0)
		     (bm25 tf (gethash ngram df) (gethash key1 l-doc-hash) l-avg :n number-of-all-docs))))
    ;; sort and paginate
    ;; we currently remove all values < 26.0 to increase accuracy,
    ;; 26.0 is the value i found to weed out all close but not same results
    ;; you can improve on this; this doesn't handle spelling errors very well.
    (let* ((alist (remove-if (lambda (a) (< a 26.0)) (zsort:quicksort (hash->alist bm25) #'> :key #'cdr) :key #'cdr))
	   (alist-len (length alist))
	   (page-size 10)
	   (start-index (* page-size (1- page-number)))
	   (end-index (min (+ start-index page-size) alist-len))
	   (sublist (when (< start-index alist-len) (subseq alist start-index end-index))))
      (cons alist-len (with-transaction (tr *db*)
			(loop for (kv . bm25) in sublist
			      collect (let ((data (future-value (transaction-get tr (apply #'tuple-encode `("fts" "data" ,@kv))))))
					(cons kv (restore data)))))))))
