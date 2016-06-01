#! /usr/bin/env racket
#lang racket

;;; This will become a MassMine Analysis and Data Manipulation command
;;; line tool 

(require math json)

;;; Control parameters --------------------------------------
(define verbose? (make-parameter #f))
;;; Location of cache files
(define cache-dir (make-parameter (build-path (current-directory) "mm-cache")))
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
;;; The actual cache used by mmtool's internals during runtime, as a
;;; hash
(define cache (make-parameter null))
;;; The actual meta cache used by mmtool's internals during runtime,
;;; as a hash
(define cache-meta (make-parameter null))

;;; This parameter controls which task mmtool will carry out. It is
;;; set at runtime by one of the once-any command line arguments
(define task (make-parameter #f))
;;; mmtool can work with piped data, or with a specific file. By
;;; default, it assumes a file.
(define from-file? (make-parameter #t))

(define (hashtags tweet)
  (if (not (string? tweet))
      '()
      (let ([hash-regexp #px"#[[:alpha:]][[:alnum:]_]+"])
	(regexp-match* hash-regexp tweet))))

(define (find-hashtags-by-tweet)
  (let loop ([hash-tags '()]
	     [tweet (read-json (current-input-port))])
    (if (eof-object? tweet)
	hash-tags
	(loop (cons (hashtags (hash-ref tweet 'text #f)) hash-tags)
	      (read-json (current-input-port))))))


;;; This gets things done. Primarily, this reads an input (from stdin
;;; or file) line by line and/or calls a corresponding task dependent
;;; on the user's command line argument(s)
(define (task-dispatch)
  (cond
   [(equal? (task) 'hash-tags) (display (samples->hash (flatten (find-hashtags-by-tweet))))]
   [else (displayln "You must request a specific task")]))

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

;;; Call this to save the current (presumably updated) cache to disk
(define (write-cache-meta)
  (with-output-to-file (cache-meta-file)
    (λ () (write (cache-meta)))
    #:exists 'replace))

;;; Returns the given input document's cache ID for lookup
;; (define (cache-id source-path)
;;   (let ([cache (read (cache-dir))])))

;;; Command line parsing
(define filename
  (command-line
   #:program "mmtool"
   #:once-each
   [("-v") "Verbose mode" (verbose? #t)]
   #:once-any
   [("--hash-tags") "Display hashtags" (task 'hash-tags)]
   #:args
   ([fname null])
   fname))

(define (process-data data-file)
  ;; Piped data results in no caching
  (when (null? data-file) (from-file? #f) (cache-results? #f))
  ;; Load our cache information when necessary
  (when (from-file?) 
    (cache-meta (load-cache-meta))
    (cache (load-cache))
    ;; Has the file content changed since our last run? Update the
    ;; cache if so. We also update if there is no cache for this data
    ;; file. If cache-results? is #f, nothing will be updated regardless
    ;; of what we do here.
    (define prior-date (hash-ref (cache-meta) data-file #f))
    (define current-date (file-or-directory-modify-seconds data-file))
    (when (or (not prior-date) (> current-date prior-date))
      (update-cache? #t)))

  ;; Get down to business. Carry out the requested task
  (cond
   ;; No filename was supplied at runtime. Assume piped data stream
   [(null? data-file) (task-dispatch)]
   [(file-exists? data-file) (with-input-from-file data-file (λ () (task-dispatch)))]
   [else (printf "Error: File ~a does not exist\n" data-file)])

  ;; Finish up. Update the cache if needed
  (when (and (cache-results?) (update-cache?))
    (hash-set! (cache-meta) data-file
	       (file-or-directory-modify-seconds data-file))
    (write-cache-meta)))

;;; Go!
(process-data filename)
