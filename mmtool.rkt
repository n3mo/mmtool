#! /usr/bin/env racket
#lang racket

;;; This will become a MassMine Analysis and Data Manipulation command
;;; line tool 

(require math json)

;;; Control parameters --------------------------------------
(define verbose? (make-parameter #f))
;;; Location of cache files
(define cache-dir (build-path (current-directory) "mm-cache"))
;;; Meta cache file. Contains IDs for currently cached objects. 
(define cache-meta-file (build-path cache-dir "cache"))
;;; Contents of cache items appear in this file
(define cache-contents (build-path cache-dir "cache-data"))

;;; Command line arguments
(define greeting
  (command-line
   #:program "mmtool"
   #:once-each
   [("-v") "Verbose mode" (verbose? #t)]
   #:args
   (str) str))


;;; For now, we work with data manually. All functions below that
;;; accept Twitter data will operate on (current-input-port) wherever
;;; possible/sensible

;;; Our data file (in JSON format)
;; (define input-file "sample.json")

;;; Lazy, heavy-handed method. Read the entire JSON file into memory
;;; and convert it to jsexpr
;; (define d (with-input-from-string (file->string input-file)
;; 	    (λ () (read-json (current-input-port)))))

;;; Returns hashtags from a list of Tweet data (as contained in "d"
;;; above)
;; (define (hashtags tweets)
;;   (let ([hash-regexp #px"#[[:alpha:]][[:alnum:]_]+"])
;;     (samples->hash
;;      (flatten (for/list ([tweet (map (λ (x) (hash-ref x 'text)) tweets)])
;; 		(regexp-match* hash-regexp tweet))))))

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
(define (main)
  (samples->hash (flatten (find-hashtags-by-tweet))))

;;; Returns the given input document's cache ID for lookup
(define (cache-id source-path)
  (let ([cache (read (cache-dir))])))

;;; Go!
;;; (main)


;; (printf "~a~a\n"
;; 	greeting
;; 	(if (verbose?) " to you, too!" ""))
