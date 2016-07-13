#! /usr/bin/env racket
#lang racket

;;; This will become a MassMine Analysis and Data Manipulation command
;;; line tool 

(require math json)

(include "server.rkt")

;;; Current version
(define mmtool-version "0.0.1 (2016-06-01)")

;;; Control parameters --------------------------------------
;;; Location of cache files
(define cache-dir (make-parameter (build-path (current-directory) "mm-cache")))
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

;;; This is meant to be called by the GUI server only. It returns
;;; hashtag results as an X-expression. It reads from the current
;;; input port, which should be set to the user's data file
(define (GUI-hashtags)
  (let ([hash-tags (hash-ref (cache) 'hashtags #f)]
	[f (λ (x) (sort  (hash->list (samples->hash (flatten x)))
			 (λ (x y) (> (cdr x) (cdr y)))))])
    (if (and (use-cache?) hash-tags)
	;; Use cached data
	`(table (tr (th "Hashtag") (th "Frequency"))
		,@(map (λ (x) `(tr (td ,(car x))
				   (td ,(number->string (cdr x)))))
		       (f hash-tags)))
	;; No cache. Calculate using raw data
	`(table (tr (th "Hashtag") (th "Frequency"))
		,@(map (λ (x) `(tr (td ,(car x))
				   (td ,(number->string (cdr x)))))
		       (f (find-hashtags-by-record)))))))

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
   [(equal? (task) 'GUI-hashtags) (GUI-hashtags)]
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
   ;; [("--anonymize") "Anonymize @usernames" (task 'anonymize)]
   [("--purge-cache") "Purge cache (optionally for 1 file)" (task 'purge-cache)]
   [("-v" "--version") "Version info" (task 'version)]
   #:args
   ([fname null])
   fname))

(define (process-data data-file)
  ;; Piped data results in no caching
  (when (null? data-file) (from-file? #f) (cache-results? #f))
  ;; Load our cache information when necessary
  (when (from-file?)
    (cache-key data-file)
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
