(in-package :zamsdeals.nlp)

(defparameter *stop-words*
  '("a" "an" "the" "and" "or" "but" "if" "then" "else" "when" "at" "by" "from" "for" "with" "about" "against" "between" "into" "through" "during" "before" "after" "above" "below" "to" "from" "up" "down" "in" "out" "on" "off" "over" "under" "again" "further" "then" "once" "here" "there" "all" "any" "both" "each" "few" "more" "most" "other" "some" "such" "no" "nor" "not" "only" "own" "same" "so" "than" "too" "very" "hi" "nil")
    "add nil to catch any non existent item details")

(defun remove-punctuation (str)
  (remove-if (lambda (char) (find char ".,;:!?\"'()[]{}`-_~/±"))
             str))

(defun tokenize (message)
  "during tokenizing, replace _ with space as the models might combine words with it, remove stop words in *stop-words*
when tokenising, we need to take into account the ' you want to find eg boy's, you want to link to boy and boy's, to do that we will need to run tokenise on the message after replacing ' with space, and again when we don't to capture both variants."
  (if (stringp message)
      (let* ((tokens-without-apostrophe (split-sequence:split-sequence #\Space (str:replace-all "'" " " message) :remove-empty-subseqs t))
	     (tokens-with-apostrophe (split-sequence:split-sequence #\Space (str:replace-all "_" " " message) :remove-empty-subseqs t))
	     (tokens (remove-duplicates `(,@tokens-without-apostrophe ,@tokens-with-apostrophe) :test #'equal)))
	(remove-if (lambda (word)
                     (member word *stop-words* :test #'string=))
		   (mapcar #'str:trim (mapcar #'string-downcase (mapcar #'string (mapcar #'remove-punctuation tokens))))))
      (remove-duplicates (let (acc)
			   (dolist (m message)
			     (setq acc `(,@acc ,@(tokenize m))))
			   acc)
			 :test #'equal)))

(defun generate-substring-counts (input ngram-length)
  "Generates all substrings from the input and returns an association list
   mapping each unique substring to its total count (frequency).

  Arguments:
    input : Can be a single string, a list of strings, or a vector of strings.

  Returns:
    An association list where each element is (substring . count),
    representing the total count of that substring across all input strings.
    Returns NIL if the input is invalid, empty, or contains no processable strings."

  (let ((counts-table (make-hash-table :test #'equal)))
    (labels ((process-single-string (text table)
               (when (and text (stringp text))
                 (let ((len (length text)))
                   (loop for i from 0 below len do
		     (loop for j from (+ i ngram-length) to len do
		       (let ((sub (subseq text i j)))
			 (incf (gethash sub table 0)))))))))
      (typecase input
        (string
	 (process-single-string input counts-table))
	(list
	 (dolist (item input)
	   (process-single-string item counts-table)))
        (vector
	 (loop for item across input do
	   (process-single-string item counts-table)))
        (t
	 nil))
      (if (> (hash-table-count counts-table) 0)
	  (let ((result-alist '()))
	    (maphash (lambda (key value)
		       (push (cons key value) result-alist))
		     counts-table)
	    result-alist)
	  nil))))

(defun count-terms (tokens)
  "Counts occurrences of each term in the token list."
  (let ((term-counts (make-hash-table :test 'equal)))
    (dolist (token tokens)
      (incf (gethash token term-counts 0)))
    term-counts))

(defun compute-tf (document)
  "Computes the term frequency (TF) for each term in the document."
  (let* ((tokens (tokenize document))
         (term-counts (count-terms tokens))
         (total-terms (length tokens))
         (tf (make-hash-table :test 'equal)))
    (maphash (lambda (term count)
               (setf (gethash term tf) (/ (float count) total-terms)))
             term-counts)
    tf))

(defun remove-lisp-encapsulation (input)
  "Strips the beginning ```lisp and ending ``` if they are present."
  (let ((start-marker "```lisp")
        (end-marker "```"))
    (if (and (>= (length input) (+ (length start-marker) (length end-marker)))
             (string= (subseq input 0 (length start-marker)) start-marker)
             (string= (subseq input (- (length input) (length end-marker))) end-marker))
        (subseq input (length start-marker) (- (length input) (length end-marker)))
        input)))

(defun remove-json-encapsulation (input)
  "Strips the beginning ```json and ending ``` if they are present."
  (let ((start-marker "```json")
        (end-marker "```"))
    (if (and (>= (length input) (+ (length start-marker) (length end-marker)))
             (string= (subseq input 0 (length start-marker)) start-marker)
             (string= (subseq input (- (length input) (length end-marker))) end-marker))
        (subseq input (length start-marker) (- (length input) (length end-marker)))
        input)))

(defun make-strict-json (json-str &aux (json-string (str:trim json-str)))
  "this function doesn't handle complex stuff like lists of objects, otherwise it will work fine"
  (trivia:match json-string
    ("{}" "{}")
    ("[]" "[]")
    (_
     (when (json-object-p json-string)
       (let* ((kv-lst (split-json-string-to-kv json-string))
              (kv-lst1 (mapcar (lambda (s) (str:split ":" s :limit 2)) kv-lst))
              (stream (make-string-output-stream)))
         (format stream "{")
         (loop for (k v) in kv-lst1
               do (format stream
                          (trivia:match (list (escaped-p k) (or (json-list-p v) (json-object-p v)))
                            ((list t t) "~a:~a,")
                            ((list nil t) "~s:~a,")
                            (_ "~s:~s,"))
                          (str:trim k)
                          (cond
                            ((json-object-p v) (make-strict-json v))
                            ((json-list-p v) (make-strict-json-list v))
                            (t (str:trim v)))))
         (let* ((s (get-output-stream-string stream))
                (length-s (length s)))
           (str:replace-all "\\\"" "" (format nil "~a}" (subseq s 0 (1- length-s))))))))))

(defun make-strict-json-list (json-str &aux (json-string (str:trim json-str)))
  (trivia:match json-string
    ("[]" "[]")
    (_
     (when (json-list-p json-string)
       (let* ((elements (split-json-string-to-elements json-string))
              (stream (make-string-output-stream)))
         (format stream "[")
         (loop for elem in elements
               do (format stream "~a,"
                          (cond
                            ((json-object-p elem) (make-strict-json elem))
                            ((json-list-p elem) (make-strict-json-list elem))
                            (t (str:trim elem)))))
         (let* ((s (get-output-stream-string stream))
                (length-s (length s)))
           (str:replace-all "\\\"" "" (format nil "~a]" (subseq s 0 (1- length-s))))))))))

(defun json-object-p (json-str &aux (json-string (str:trim json-str)))
  "Checks whether a string is a JSON object by checking if it starts with '{' and ends with '}'."
  (and (>= (length json-string) 2)
       (char= (char json-string 0) #\{)
       (char= (char json-string (1- (length json-string))) #\})))

(defun json-list-p (json-str &aux (json-string (str:trim json-str)))
  "Checks whether a string is a JSON list by checking if it starts with '[' and ends with ']'."
  (and (>= (length json-string) 2)
       (char= (char json-string 0) #\[)
       (char= (char json-string (1- (length json-string))) #\])))

(defun remove-json-object-braces (json-string)
  "Removes the outer braces from a JSON object string and returns the content inside."
  (if (and (json-object-p json-string)
           (>= (length json-string) 2))
      (subseq json-string 1 (1- (length json-string)))
      json-string))

(defun remove-json-list-brackets (json-string)
  "Removes the outer brackets from a JSON list string and returns the content inside."
  (if (and (json-list-p json-string)
           (>= (length json-string) 2))
      (subseq json-string 1 (1- (length json-string)))
      json-string))

(defun split-json-string-to-kv (json-string)
  "Splits a JSON object string into key-value pairs. Handles strings with commas and nested objects."
  (let (kv acc at-object at-list at-string escape
           (no-braces (remove-json-object-braces json-string)))
    (loop for i from 0 below (length no-braces)
          for char = (char no-braces i)
          do (trivia:match char
               (#\" (if at-string
                        (unless escape
                          (setq at-string nil))
                      (setq at-string t))
                   (setq escape nil)
                   (setq acc `(,@acc ,char)))
               (#\\ (if (and at-string (not escape))
                        (setq escape t)
                      (setq escape nil))
                   (setq acc `(,@acc ,char)))
               (#\{ (unless at-string
                      (setq at-object t))
                   (setq acc `(,@acc ,char)))
               (#\[ (unless at-string
                      (setq at-list t))
                   (setq acc `(,@acc ,char)))
               (#\} (unless at-string
                      (setq at-object nil))
                   (setq acc `(,@acc ,char))
                   (when (and (not at-string) (not at-list))
                     (setq kv `(,@kv ,(chars-to-string acc)))
                     (setq acc nil)))
               (#\] (unless at-string
                      (setq at-list nil))
                   (setq acc `(,@acc ,char))
                   (when (and (not at-string) (not at-object))
                     (setq kv `(,@kv ,(chars-to-string acc)))
                     (setq acc nil)))
               (#\, (if (and (not at-string) (not at-object) (not at-list))
                        (progn
                          (setq kv `(,@kv ,(chars-to-string acc)))
                          (setq acc nil))
                      (setq acc `(,@acc ,char))))
               (_ (setq acc `(,@acc ,char)))))
    (remove-if (lambda (s) (equal "" s)) (setq kv `(,@kv ,(chars-to-string acc))))))

(defun split-json-string-to-elements (json-string)
  "Splits a JSON list string into elements. Handles strings with commas and nested objects/lists."
  (let (elements acc at-object at-list at-string escape
                 (no-brackets (remove-json-list-brackets json-string)))
    (loop for i from 0 below (length no-brackets)
          for char = (char no-brackets i)
          do (trivia:match char
               (#\" (if at-string
                        (unless escape
                          (setq at-string nil))
                      (setq at-string t))
                   (setq escape nil)
                   (setq acc `(,@acc ,char)))
               (#\\ (if (and at-string (not escape))
                        (setq escape t)
                      (setq escape nil))
                   (setq acc `(,@acc ,char)))
               (#\{ (unless at-string
                      (setq at-object t))
                   (setq acc `(,@acc ,char)))
               (#\[ (unless at-string
                      (setq at-list t))
                   (setq acc `(,@acc ,char)))
               (#\} (unless at-string
                      (setq at-object nil))
                   (setq acc `(,@acc ,char)))
               (#\] (unless at-string
                      (setq at-list nil))
                   (setq acc `(,@acc ,char)))
               (#\, (if (and (not at-string) (not at-object) (not at-list))
                        (progn
                          (setq elements `(,@elements ,(chars-to-string acc)))
                          (setq acc nil))
                      (setq acc `(,@acc ,char))))
               (_ (setq acc `(,@acc ,char)))))
    (remove-if (lambda (s) (equal "" s)) (setq elements `(,@elements ,(chars-to-string acc))))))

(defun chars-to-string (lst)
  "Convert a list of characters to a string."
  (str:trim (funcall #'concatenate 'string lst)))

(defun escaped-p (str)
  "Determine if a string contains escaped characters."
  (str:contains? "\"" str))

(defun remove-newlines (str)
  "Remove all newlines from the given string STR."
  (let ((result (with-output-to-string (out)
                  (loop for char across str
                        unless (char= char #\Newline)
                        do (write-char char out)))))
    result))

;; TF/IDF

