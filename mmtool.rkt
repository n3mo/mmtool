#! /usr/bin/env racket
#lang racket

;;; This will become a MassMine Analysis and Data Manipulation command
;;; line tool 

(require json)
(require (only-in math
		  count-samples
		  samples->hash))
;;; I want date->string from srfi/19, not racket/date. But I want the
;;; other stuff
(require (except-in racket/date date->string))
(require (except-in srfi/19 current-date))

;; (include "server.rkt")
(include "gui.rkt")

;;; Current version
(define mmtool-version "0.0.1 (2016-06-01)")

;;; Control parameters --------------------------------------
;;; Location of cache files
(define cache-dir (make-parameter (build-path (current-directory) "mm-cache")))
;;; Cached images (such as data figures) are saved here
(define cache-img-dir (make-parameter (build-path (cache-dir) "img")))
;;; Active data file (persists across threads and instances of mmtool
(define active-file-path (make-parameter (build-path (cache-dir) "adf")))
;;; Meta cache file. Contains IDs for currently cached objects. 
(define cache-meta-file (make-parameter (build-path (cache-dir) "cache")))
;;; Contents of cache items appear in this file
(define cache-file (make-parameter (build-path (cache-dir) "cache-data")))
;;; By default, mmtool caches results where appropriate. This only
;;; works when the user supplies a filename at runtime. For piped
;;; streaming data input, caching is disabled (set to #f)
(define cache-results? (make-parameter #t))
;;; Should the current file's cache be updated? This should occur when
;;; the file has changed since the last run
(define update-cache? (make-parameter #f))
;;; Use the cache? If the file has changed since the last run, we
;;; should recompute any requested tasks
(define use-cache? (make-parameter #t))
;;; If the cache gets updated by anything, we must save it before
;;; exiting the program. Anything that updates the cache should set
;;; this to #t
(define save-cache? (make-parameter #f))
;;; The actual cache used by mmtool's internals during runtime, as a
;;; hash. This contqins all cached data, from any data files processed
;;; from within the calling directory.
(define full-cache (make-parameter null))
;;; Cache key. Caches are hash tables, with the file name as the key.
(define cache-key (make-parameter null))
;; The cache for the current file. This hash is a nested hash
;; contained within full-cache
(define cache (make-parameter (make-hash)))
;;; The actual meta cache used by mmtool's internals during runtime,
;;; as a hash
(define cache-meta (make-parameter (make-hash)))
;;; This parameter controls which task mmtool will carry out. It is
;;; set at runtime by one of the once-any command line arguments
(define task (make-parameter #f))
;;; mmtool can work with piped data, or with a specific file. By
;;; default, it assumes a file.
(define from-file? (make-parameter #t))
;;; Is mmtool running as a gui?
(define gui? (make-parameter #f))
;;; When running in GUI mode, results (typically X-expressions) are
;;; saved into this parameter for passing back to the web server
(define gui-result (make-parameter #f))
;;; Unit of time for binning in time series analyses. Default is
;;; "minute" 
(define time-units (make-parameter 'second))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               Helper Functions
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Data type heuristic. This function reads a sample from a data file
;;; (from current-input port), and uses heuristics to attempt to
;;; identify what kind of MassMine data it contains. For instance, if
;;; a user collected data with MassMine using the "twitter-stream"
;;; task, this function should identify the resulting data file as
;;; "Twitter Stream Data" (or similar)
(define (data-type? #:attempts [attempts 5])
  (let ([record (read-json (current-input-port))])
    (when (eof-object? record) "<Unknown Data Source>")
    (if (list? record)
	;; JSON data was in an array. Work with what is now a list
	(let loop ([num-attempts 1]
		   [d record])
	  (when (or (> num-attempts attempts)
		    (null? d))
	    "<Unknown Data Source>")
	  (let ([result (data-type-heuristic (car d))])
	    (if result
		result
		(loop (add1 num-attempts) (cdr d)))))
	;; JSON data is line-oriented. Work by reading the file
	(let loop ([num-attempts 1]
		   [d record])
	  (if (or (> num-attempts attempts)
		    (eof-object? d))
	      "<Unknown Data Source>"
	      (let ([result (data-type-heuristic d)])
		(if result
		    result
		    (loop (add1 num-attempts)
			  (read-json (current-input-port))))))))))

(define (data-type-heuristic record)
  (cond
   ;; Twitter data sources
   [(and (hash-ref record 'id #f)
	 (hash-ref record 'created_at #f)
	 (hash-ref record 'text #f))
    "Twitter Tweets"]
   [(and (hash-ref record 'woeid #f)
	 (hash-ref record 'tweet_volume #f))
    "Twitter Trends"]
   [(and (hash-ref record 'description #f)
	 (hash-ref record 'screen_name #f)
	 (hash-ref record 'followers_count #f))
    "Twitter Friends/Followers"]
   [(and (hash-ref record 'placeType #f)
	 (hash-ref record 'countryCode #f))
    "Twitter Locations"]
   ;; Google data sources
   [(and (hash-ref record 'location #f)
	 (hash-ref record 'query #f))
    "Google Country Trends"]
   [(hash-ref record 'formattedTraffic #f)
    "Google Trends"]
   ;; Tumblr data sources
   [(hash-ref record 'total_posts #f)
    "Tumblr Info"]
   [(hash-ref record 'featured_in_tag #f)
    "Tumblr Tag Search"]
   [(hash-ref record 'blog_name #f)
    "Tumblr Posts"]
   ;; Web URL data sources
   [(and (hash-ref record 'text #f)
	 (hash-ref record 'url #f)
	 (hash-ref record 'timestamp #f))
    "Web Pages"]
   ;; Wikipedia data sources
   [(and (hash-ref record 'ns #f)
	 (hash-ref record 'title #f)
	 (hash-ref record 'source #f))
    "Wikipedia Links"]
   [(and (hash-ref record 'size #f)
	 (hash-ref record 'wordcount #f))
    "Wikipedia Search"]
   [(and (hash-ref record 'ns #f)
	 (hash-ref record 'pageid #f))
    "Wikipedia Text"]
   [(and (hash-ref record 'date #f)
	 (hash-ref record 'views #f))
    "Wikipedia Page Views"]
   ;; We've run out of options at this point and still can't
   ;; identify the data source. Return #f and let the calling function
   ;; decide what to do
   [else #f]))

;; taken from data-science
;;; Generating discrete histograms of (sorted!) binned samples should
;;; be easier. The following generates sorted bins suitable for
;;; plotting with `discrete-histogram`. This function is similar to
;;; `samples->hash` but does not return a hash or dotted pairs. The
;;; return value is a list of (key value) pairs sorted by keys.
;;; Example: '(3 3 2 1 4 4 4) => '((1 1) (2 1) (3 2) (4 3))
(define (sorted-counts lst)
  (let-values ([(keys values) (count-samples lst)])
    (sort (map list keys values)
	  (λ (x y) (if (number? (car x))
		       (< (car x) (car y))
		       (string<? (car x) (car y)))))))

;;; This function reads line-oriented JSON (as output by massmine),
;;; and packages it into an array. This isn't how we want to do
;;; business for analysis (as we must read the entire file into
;;; memory). But this is useful for arranging our data properly for
;;; the JSON viewer in the GUI app. If `head` is a number, only that
;;; many JSON lines are returned in the resulting array
;; (define (json-lines->json-array #:head [head #f])
;;   (let loop ([num 0]
;;              [json-array '()]
;; 	     [record (read-json (current-input-port))])
;;     (if (or (eof-object? record)
;;             (and head (>= num head)))
;; 	(jsexpr->string json-array)
;; 	(loop (add1 num) (cons record json-array)
;; 	      (read-json (current-input-port))))))
(define (json-lines->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()]
	     [record (read-json (current-input-port))])
    (if (or (eof-object? record)
            (and head (>= num head)))
	json-array
	(loop (add1 num) (cons record json-array)
	      (read-json (current-input-port))))))

;; This is used to convert a raw JSON file into an internal racket
;; data structure. It does so by only reading in one line of the
;; original file at a time (to avoid consuming a lot of RAM). It keeps
;; only those fields necessary for analysis (as determined by the data
;; file type -- (data-type?)). This considerably reduces the size of
;; the original data set, while enabling us to work within RAM. The
;; goal of this is to enable parallelism across multiple processing
;; cores. For very large files, this should be avoided. `file` should
;; be a string or path
(define (json->internal file)
  ;; What we extract depends on the data type (e.g., is this tweet
  ;; data?). The extractor amounts to a series of hash-ref calls to
  ;; grab the necessary data from the JSON file. The data type is
  ;; determined by (data-type?)
  (let* ([type (with-input-from-file file (λ () (data-type?)))]
	 [extractor
	  (cond [(string=? type "Twitter Tweets")
		 (λ (x)
		   (hash 'text (hash-ref x 'text)
			 'created_at (hash-ref x 'created_at)))]
		[else
		 (λ (x)
		   (hash 'text (hash-ref x 'text)
			 'created_at (hash-ref x 'created_at)))])])
    (with-input-from-file file
      (λ ()
	(let loop ([result '()]
		   [record (read-json (current-input-port))])
	  (if (eof-object? record)
	      (begin
		;; We have a list of lists, 1 for each input record. Each sub-list
		;; is a list contains hashtags present in that record, or an empty
		;; list if none. Save this value to our cache
		(when (cache-results?)
		  (hash-set! (cache) 'internal result)
		  (save-cache? #t))
		;; Return value
		result)
	      (loop (cons (extractor record) result)
		    (read-json (current-input-port)))))))))

(define (hashtags record)
  (if (not (string? record))
      '()
      (let ([hash-regexp #px"#[[:alpha:]][[:alnum:]_]+"])
	(regexp-match* hash-regexp record))))

(define (usernames record)
  (if (not (string? record))
      '()
      (let ([hash-regexp #px"@[[:alpha:]][[:alnum:]_]+"])
	(regexp-match* hash-regexp record))))

;;; Use this for processing files line-by-line. This is great (but
;;; slow) for extrememly large data files.
(define (find-hashtags-by-record)
  (let loop ([hash-tags '()]
	     [record (read-json (current-input-port))])
    (if (eof-object? record)
	(begin
	  ;; We have a list of lists, 1 for each input record. Each sub-list
	  ;; is a list contains hashtags present in that record, or an empty
	  ;; list if none. Save this value to our cache
	  (when (cache-results?)
	    (hash-set! (cache) 'hashtags hash-tags)
	    (save-cache? #t))
	  ;; Return value
	  hash-tags)
	(loop (cons (hashtags (hash-ref record 'text #f)) hash-tags)
	      (read-json (current-input-port))))))

;;; Use this for processing files that have been internalized in
;;; Racket data structure with (JSON->internal). This is preferred
;;; when possible because the data is already in memory, reduced
;;; dramatically in size, and can be parallelized with futures. `lst`
;;; should be a list of hashes. 
(define (gui-find-hashtags-by-record lst)
  (let ([tags (map (λ (x) (hashtags (hash-ref x 'text #f))) lst)])
    (when (cache-results?)
      (hash-set! (cache) 'hashtags tags)
      (save-cache? #t))
    tags))

;;; Use this for processing files line-by-line. This is great (but
;;; slow) for extrememly large data files.
(define (find-usernames-by-record)
  (let loop ([user-names '()]
	     [record (read-json (current-input-port))])
    (if (eof-object? record)
	(begin
	  ;; We have a list of lists, 1 for each input record. Each sub-list
	  ;; is a list contains hashtags present in that record, or an empty
	  ;; list if none. Save this value to our cache
	  (when (cache-results?)
	    (hash-set! (cache) 'usernames user-names)
	    (save-cache? #t))
	  ;; Return value
	  user-names)
	(loop (cons (usernames (hash-ref record 'text #f)) user-names)
	      (read-json (current-input-port))))))

;;; Use this for processing files that have been internalized in
;;; Racket data structure with (JSON->internal). This is preferred
;;; when possible because the data is already in memory, reduced
;;; dramatically in size, and can be parallelized with futures. `lst`
;;; should be a list of hashes. 
(define (gui-find-usernames-by-record lst)
  (let ([users (map (λ (x) (usernames (hash-ref x 'text #f))) lst)])
    (when (cache-results?)
      (hash-set! (cache) 'usernames users)
      (save-cache? #t))
    users))


;;; Call this to display hash-tags for the --hash-tags task. This
;;; function is designed for printing out results during command line
;;; usage. For GUI requests, use GUI-hashtags
(define (display-hashtags)
  (let ([hash-tags (hash-ref (cache) 'hashtags #f)]
	[f (λ (x) (sort  (hash->list (samples->hash (flatten x)))
			 (λ (x y) (> (cdr x) (cdr y)))))])
    (if (and (use-cache?) hash-tags)
	(for-each (λ (x) (printf "~a: ~a\n" (car x) (cdr x)))
		  (f hash-tags))
	(for-each (λ (x) (printf "~a: ~a\n" (car x) (cdr x)))
		  (f (find-hashtags-by-record))))))

;;; This method is used by the web-server gui to find hashtags
(define (web-GUI-hashtags)
  (let ([hash-tags (hash-ref (cache) 'hashtags #f)]
	[f (λ (x) (sort  (hash->list (samples->hash (flatten x)))
			 (λ (x y) (> (cdr x) (cdr y)))))])
    (if (and (use-cache?) hash-tags)
	;; Use cached data
	`(table ((class "table table-striped table-hover"))
		(tr (th "Hashtag") (th "Frequency"))
		,@(map (λ (x) `(tr (td ,(car x))
				   (td ,(number->string (cdr x)))))
		       (f hash-tags)))
	;; No cache. Calculate using raw data
	`(table ((class "table table-striped table-hover"))
		(tr (th "Hashtag") (th "Frequency"))
		,@(map (λ (x) `(tr (td ,(car x))
				   (td ,(number->string (cdr x)))))
		       (f (find-hashtags-by-record)))))))

;;; This is meant to be called by the GUI server only. It returns
;;; hashtag results as an X-expression. It reads from the current
;;; input port, which should be set to the user's data file
(define (GUI-hashtags lst)
  (let ([hash-tags (hash-ref (cache) 'hashtags #f)]
	[f (λ (x) (sort  (hash->list (samples->hash (flatten x)))
			 (λ (x y) (> (cdr x) (cdr y)))))])
    (if (and (use-cache?) hash-tags)
	;; Use cached data
        (f hash-tags)
	;; No cache. Calculate using raw data
        (f (gui-find-hashtags-by-record lst)))))

;;; Call this to display @usernames for the --user-mentions task
(define (display-user-mentions)
  (let ([user-names (hash-ref (cache) 'usernames #f)]
	[f (λ (x) (sort  (hash->list (samples->hash (flatten x)))
			 (λ (x y) (> (cdr x) (cdr y)))))])
    (if (and (use-cache?) user-names)
	(for-each (λ (x) (printf "~a: ~a\n" (car x) (cdr x)))
		  (f user-names))
	(for-each (λ (x) (printf "~a: ~a\n" (car x) (cdr x)))
		  (f (find-usernames-by-record))))))

;;; This is meant to be called by the GUI server only. It returns
;;; @user-mentions results as an X-expression. It reads from the
;;; current input port, which should be set to the user's data file
(define (GUI-user-mentions lst)
  (let ([user-names (hash-ref (cache) 'usernames #f)]
	[f (λ (x) (sort  (hash->list (samples->hash (flatten x)))
			 (λ (x y) (> (cdr x) (cdr y)))))])
    (if (and (use-cache?) user-names)
	;; Use cached data
        (f user-names)
	;; No cache. Calculate using raw data
        (f (gui-find-usernames-by-record lst)))))

;;; Original method for web gui
;; (define (web-GUI-user-mentions)
;;   (let ([user-names (hash-ref (cache) 'usernames #f)]
;; 	[f (λ (x) (sort  (hash->list (samples->hash (flatten x)))
;; 			 (λ (x y) (> (cdr x) (cdr y)))))])
;;     (if (and (use-cache?) user-names)
;; 	;; Use cached data
;; 	`(table ((class "table table-striped table-hover"))
;; 		(tr (th "User Name") (th "Frequency"))
;; 		,@(map (λ (x) `(tr (td ,(car x))
;; 				   (td ,(number->string (cdr x)))))
;; 		       (f user-names)))
;; 	;; No cache. Calculate using raw data
;; 	`(table ((class "table table-striped table-hover"))
;; 		(tr (th "User Name") (th "Frequency"))
;; 		,@(map (λ (x) `(tr (td ,(car x))
;; 				   (td ,(number->string (cdr x)))))
;; 		       (f (find-usernames-by-record)))))))

;;; Get raw timestamp from JSON. Use this for extremely large files
;;; only. User the faster gui-json-get-raw-timestamps for smaller
;;; files that can be stored in RAM. 
(define (json-get-raw-timestamps)
  (let ([tmp (hash-ref (cache) 'timestamps #f)])
    (if tmp
	tmp
	(let loop ([tstamps '()]
		   [record (read-json (current-input-port))])
	  (if (eof-object? record)
	      (begin
		;; Update the cache?
		(when (cache-results?)
		  (hash-set! (cache) 'timestamps tstamps)
		  (save-cache? #t))
		;; Return raw timestamps
		tstamps)
	      (loop (cons
		     (hash-ref record 'created_at)
		     tstamps)
		    (read-json (current-input-port))))))))

;;; Use this for smaller data that has been internalized into Racket
;;; structures with JSON->internal. Use the non-gui version for large
;;; files. 
(define (gui-json-get-raw-timestamps lst)
  (let ([tstamps (map (λ (x) (hash-ref x 'created_at)) lst)])
    (when (cache-results?)
      (hash-set! (cache) 'timestamps tstamps)
      (save-cache? #t))
    tstamps))

;; This reads json one item at a time, and keeps a record of its
;; timestamp
(define (json-timestamps [lst #f])
  (let ([tstamps (if lst
		     (gui-json-get-raw-timestamps lst)
		     (json-get-raw-timestamps))])
    (map (λ (x)
	   (string->date x
			 "~a ~b ~d ~H:~M:~S ~z ~Y"))
	 tstamps)))

;;; Time series. Simple counts of data records by unit time. Units can
;;; be second, minute, hour, day, month, or year as a symbol
(define (get-time-series [lst #f] #:units [units 'minute])
  (let ([timestamps (json-timestamps lst)]
	[time-format
	 (cond
	  [(equal? units 'second) "~Y ~m ~d ~H:~M:~S"]
	  [(equal? units 'minute) "~Y ~m ~d ~H:~M"]
	  [(equal? units 'hour) "~Y ~m ~d ~H"]
	  [(equal? units 'day) "~Y ~m ~d"]
	  [(equal? units 'month) "~Y ~m"]
	  [(equal? units 'year) "~Y"]
	  [else "~Y ~m ~d ~H:~M"])])
    ;; Return
    (sorted-counts
     (map (λ (x) (date->string x time-format)) timestamps))))

;;; Pretty printer for command line output. See `get-time-series` for
;;; retrieving Racket data
(define (time-series #:units [units 'minute])
  (let ((result (get-time-series #:units units)))
    (for-each (λ (x) (printf "\"~a\": ~a\n" (first x) (second x)))
	      result)))

(define (plot-time-series #:units [units 'minute])
  ;; Expensive load time. Only load this when needed
  (local-require plot)
  (let ([d (get-time-series #:units units)]
	[outfile (build-path (current-directory)
		 	     (string-append filename
		 			    "-time-series.png"))]
	[time-format
	 (cond
	  [(equal? units 'second) "~Y ~m ~d ~H:~M:~S"]
	  [(equal? units 'minute) "~Y ~m ~d ~H:~M"]
	  [(equal? units 'hour) "~Y ~m ~d ~H"]
	  [(equal? units 'day) "~Y ~m ~d"]
	  [(equal? units 'month) "~Y ~m"]
	  [(equal? units 'year) "~Y"]
	  [else "~Y ~m ~d ~H:~M"])]
	[ticks-format
	 (cond
	  [(equal? units 'second) '("~H:~M:~S")]
	  [(equal? units 'minute) '("~H:~M")]
	  [(equal? units 'hour) '("~H")]
	  [(equal? units 'day) '("~d")]
	  [(equal? units 'month) '("~m")]
	  [(equal? units 'year) '("~Y")]
	  [else "~H:~M:~S"])])
    (parameterize ([plot-x-ticks (date-ticks #:formats ticks-format)]
		   [plot-width 960]
		   [plot-height 540])
      (plot-file (list
		  (lines
		   (map vector
			(map (λ (x) (date->seconds (string->date (first x) time-format))) d)
			(map second d))
		   #:color "DodgerBlue"
		   #:width 3)
		  (tick-grid))
		 ;; Output file path
		 outfile
		 #:x-label "Time"
		 #:y-label "Frequency"))
    (printf "Figure saved to file ~a\n" outfile)))

;;; Original web GUI version of plot-time-series
;; (define (web-GUI-plot-time-series #:units [units 'minute])
;;   ;; Expensive load time. Only load this when needed
;;   (local-require plot)
;;   (let ([d (get-time-series #:units units)]
;; 	[outfile (build-path (cache-img-dir)
;; 		 	     (string-append (active-data-file)
;; 		 			    "-time-series.png"))]
;; 	[time-format
;; 	 (cond
;; 	  [(equal? units 'second) "~Y ~m ~d ~H:~M:~S"]
;; 	  [(equal? units 'minute) "~Y ~m ~d ~H:~M"]
;; 	  [(equal? units 'hour) "~Y ~m ~d ~H"]
;; 	  [(equal? units 'day) "~Y ~m ~d"]
;; 	  [(equal? units 'month) "~Y ~m"]
;; 	  [(equal? units 'year) "~Y"]
;; 	  [else "~Y ~m ~d ~H:~M"])]
;; 	[ticks-format
;; 	 (cond
;; 	  [(equal? units 'second) '("~H:~M:~S")]
;; 	  [(equal? units 'minute) '("~H:~M")]
;; 	  [(equal? units 'hour) '("~H")]
;; 	  [(equal? units 'day) '("~d")]
;; 	  [(equal? units 'month) '("~m")]
;; 	  [(equal? units 'year) '("~Y")]
;; 	  [else "~H:~M:~S"])])
;;     (parameterize ([plot-x-ticks (date-ticks #:formats ticks-format)]
;; 		   [plot-width 960]
;; 		   [plot-height 540])
;;       (plot-file (list
;; 		  (lines
;; 		   (map vector
;; 			(map (λ (x) (date->seconds (string->date (first x) time-format))) d)
;; 			(map second d))
;; 		   #:color "DodgerBlue"
;; 		   #:width 3)
;; 		  (tick-grid))
;; 		 ;; (build-path (cache-img-dir)
;; 		 ;; 	     (string-append filename
;; 		 ;; 			    "-time-series.png"))
;; 		 outfile
;; 		 #:x-label "Time"
;; 		 #:y-label "Frequency"))
;;     ;; Return x-expression
;;     `(img ((src ,(string-append
;; 		  "/mm-cache/img/"
;; 		  (first (reverse
;; 			  (string-split
;; 			   (path->string outfile)
;; 			   "/")))))
;; 	   (alt "Time Series")
;; 	   (class "img-responsive")))))

;;; Plot time series using the gui
(define (GUI-plot-time-series lst #:units [units 'minute])
  ;; Expensive load time. Only load this when needed
  (local-require plot)
  (let ([d (get-time-series lst #:units units)]
	[time-format
	 (cond
	  [(equal? units 'second) "~Y ~m ~d ~H:~M:~S"]
	  [(equal? units 'minute) "~Y ~m ~d ~H:~M"]
	  [(equal? units 'hour) "~Y ~m ~d ~H"]
	  [(equal? units 'day) "~Y ~m ~d"]
	  [(equal? units 'month) "~Y ~m"]
	  [(equal? units 'year) "~Y"]
	  [else "~Y ~m ~d ~H:~M"])]
	[ticks-format
	 (cond
	  [(equal? units 'second) '("~H:~M:~S")]
	  [(equal? units 'minute) '("~H:~M")]
	  [(equal? units 'hour) '("~H")]
	  [(equal? units 'day) '("~d")]
	  [(equal? units 'month) '("~m")]
	  [(equal? units 'year) '("~Y")]
	  [else "~H:~M:~S"])])
    (parameterize ([plot-x-ticks (date-ticks #:formats ticks-format)]
		   [plot-width 400]
		   [plot-height 300])
      (plot-bitmap
       (list
        (lines
         (map vector
              (map (λ (x) (date->seconds (string->date (first x) time-format))) d)
              (map second d))
         #:color "DodgerBlue"
         #:width 3)
        (tick-grid))
       #:x-label "Time"
       #:y-label "Frequency"))))

;;; This gets things done. Primarily, this reads an input (from stdin
;;; or file) line by line and/or calls a corresponding task dependent
;;; on the user's command line argument(s)
(define (task-dispatch)
  (cond
   ;; [(equal? (task) 'gui) (begin (gui? #t) (start-gui))]
   [(equal? (task) 'hash-tags) (display-hashtags)]
   [(equal? (task) 'user-mentions) (display-user-mentions)]
   [(equal? (task) 'purge-cache) (purge-cache)]
   [(equal? (task) 'version) (print-version)]
   [(equal? (task) 'time-series) (time-series #:units (time-units))]
   [(equal? (task) 'plot-time-series) (plot-time-series #:units (time-units))]
   [(equal? (task) 'GUI-hashtags) (GUI-hashtags)]
   [(equal? (task) 'GUI-user-mentions) (GUI-user-mentions)]
   [(equal? (task) 'GUI-plot-time-series) (GUI-plot-time-series #:units (time-units))]
   ;; [else (begin (gui? #t) (start-gui))]
   ))

;;; Load cache meta-data. If the cache meta file doesn't exist, create
;;; it.
(define (load-cache-meta)
  (if (file-exists? (cache-meta-file))
      (with-input-from-file (cache-meta-file)
	(λ () (hash-copy (read (current-input-port)))))
      ;; Cache doesn't exist. Create an empty cache
      (begin
	(unless (file-exists? (cache-dir)) (make-directory (cache-dir)))
	(unless (file-exists? (cache-img-dir)) (make-directory (cache-img-dir)))
	(with-output-to-file (cache-meta-file)
	  (λ () (write (make-hash))))
	;; We created a cache file for next time. But we also need a
	;; fresh, empty cache this first time
	(make-hash))))

;;; Call this to load the cache for the current input file. Returns a
;;; hash of the cached data if available. Returns an empty hash
;;; otherwise. 
(define (load-cache)
  (if (file-exists? (cache-file))
      (with-input-from-file (cache-file)
	(λ () (hash-copy (read (current-input-port)))))
      ;; Cache doesn't exist. Create an empty cache
      (begin
	;;(unless (file-exists? (cache-dir)) (make-directory (cache-dir)))
	(with-output-to-file (cache-file)
	  (λ () (write (make-hash))))
	;; We created a cache file for next time. But we also need a
	;; fresh, empty cache this first time
	(make-hash))))

;;; Write cache. Call this only when the current file's cache has
;;; changed
(define (write-cache #:cache-set? [cache-set? #f])
  (unless cache-set? (hash-set! (full-cache) (cache-key) (cache)))
  (with-output-to-file (cache-file)
    (λ () (write (full-cache)))
    #:exists 'replace))

;;; Call this to save the current (presumably updated) cache to disk
(define (write-cache-meta)
  (with-output-to-file (cache-meta-file)
    (λ () (write (cache-meta)))
    #:exists 'replace))

;;; Purge cache. This function will purge the cache contents for
;;; data-file. If data-file is null (the user passed no filename at
;;; runtime), then purge the entire cache!
(define (purge-cache)
  (if (null? (cache-key))
      (begin
	(displayln "Purging entire cache")
	;; Purge meta cache info
	(with-output-to-file (cache-meta-file)
	  (λ () (write (make-hash)))
	  #:exists 'replace)
	;; Purge cache contents
	(with-output-to-file (cache-file)
	  (λ () (write (make-hash)))
	  #:exists 'replace))
      ;; User supplied a file name at runtime. Only purge data for the
      ;; specified file
      (begin
	(printf "Purging cache for ~a\n" (cache-key))
	(hash-remove! (cache-meta) (cache-key))
	(hash-remove! (full-cache) (cache-key))
	(write-cache #:cache-set? #t)
	(write-cache-meta))))

;; Prints the current version of massmine, and useful info
(define (print-version)
  (displayln (string-append "mmtool " mmtool-version))
  (displayln "https://github.com/n3mo/mmtool")
  (newline)
  (displayln "Copyright (C) 2016 Nicholas M. Van Horn")
  (displayln "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.")
  (displayln "This is free software: you are free to change and redistribute it.")
  (displayln "There is NO WARRANTY, to the extent permitted by law."))

;;; Command line parsing
(define filename
  (command-line
   #:program "mmtool"
   ;; #:once-each
   #:once-any
   [("--gui") "Run graphical user interface" (gui? #t)]
   [("--hash-tags") "Display #hashtags" (task 'hash-tags)]
   [("--user-mentions") "Display @usernames" (task 'user-mentions)]
   [("--time-series") "Data frequency across time" (task 'time-series)]
   [("--plot-time-series") "Plot data frequency across time" (task 'plot-time-series)]
   ;; [("--anonymize") "Anonymize @usernames" (task 'anonymize)]
   [("--purge-cache") "Purge cache (optionally for 1 file)" (task 'purge-cache)]
   [("-v" "--version") "Version info" (task 'version)]
   #:args
   ([fname null]
    [options null])
   fname))

(define (process-data data-file)
  ;; Piped data results in no caching
  (when (null? data-file) (from-file? #f) (cache-results? #f))
  ;; Load our cache information when necessary
  (when (from-file?)
    (cache-key (path->string data-file))
    (cache-meta (load-cache-meta))
    (full-cache (load-cache))
    (cache (hash-copy (hash-ref (full-cache) (cache-key) (make-hash))))
    ;; Has the file content changed since our last run? Update the
    ;; cache if so. We also update if there is no cache for this data
    ;; file. If cache-results? is #f, nothing will be updated regardless
    ;; of what we do here.
    (define prior-date (hash-ref (cache-meta) (cache-key) #f))
    (define current-date (file-or-directory-modify-seconds data-file))
    (when (or (not prior-date) (> current-date prior-date))
      (update-cache? #t)
      (use-cache? #f)))

  ;; Get down to business. Carry out the requested task
  (cond
   ;; No filename was supplied at runtime. Assume piped data stream
   [(null? data-file) (task-dispatch)]
   ;; We're running as a GUI. That means we must catch the result
   ;; (probably in X-expression format) for displaying through the
   ;; web server
   [(gui?)
    (gui-result
     (with-input-from-file data-file (λ () (task-dispatch))))]
   ;; File supplied from command line
   [(file-exists? data-file) (with-input-from-file data-file (λ () (task-dispatch)))]
   [else (printf "Error: File ~a does not exist\n" data-file)])

  ;; Debug
  ;; (displayln (cache-file))
  ;; (displayln (cache-meta-file))
  ;; (printf "Save cache?: ~a\n" (if (save-cache?) "True" "False"))
  ;; (printf "Update cache?: ~a\n" (if (update-cache?) "True" "False"))
  ;; (printf "Data file: ~a\n" data-file)
  ;; (printf "Null data file?: ~a" (if (null? data-file) "True" "False"))

  ;; If anything has changed the cache, save it now
  (when (save-cache?) (write-cache) (write-cache-meta))
  
  ;; Update the meta cache if needed
  (when (and (cache-results?) (update-cache?))
    (hash-set! (cache-meta) (cache-key)
	       (file-or-directory-modify-seconds data-file))
    (write-cache-meta))

  ;; If this was called by the GUI (this is different than the user
  ;; calling the gui from the command line), then we need to return
  ;; the analysis result for passing to the web server
  (when (gui?) (gui-result)))

;;; Go!
(if (gui?)
    (start-gui)
    (process-data filename))
