#! /usr/bin/env racket
#lang racket

;;; Dependencies
(require web-server/servlet
         web-server/servlet-env
	 web-server/formlets
	 web-server/page)

;;; Our response handler
(define (mmgui req)
  (user-input-form req))
 
;;; List of tasks in a selection formlet
(define mm-tasks-formlet
  (select-input '("google-country-trends" "google-trends"
		  "tumblr-auth" "tumblr-blog-info" "tumblr-posts"
		  "tumblr-tag" "twitter-auth" "twitter-followers"
		  "twitter-friends" "twitter-locations"
		  "twitter-sample" "twitter-search" "twitter-stream"
		  "twitter-trends" "twitter-trends-nohash"
		  "twitter-user" "web-text" "wikipedia-page-links"
		  "wikipedia-search" "wikipedia-text"
		  "wikipedia-views")))

; File upload formlet
(define file-upload-formlet
  (formlet
   (div ,{(required (file-upload)) . => . a-file})
   a-file))

;;; Command line input formlet
;; (define CLI-formlet
;;   (formlet
;;    (div "Command line program name:" ,{input-string . => . program-name}
;;         "Options/Flags:" ,{input-string . => . flags-and-options})
;;    (list program-name flags-and-options)))

;;; This function works on the hash returned by the web form for
;;; generating calls to massmine
(define (build-mm-command full-hash)
  (let* ((opts (filter (λ (x) (not (string=? "" (cdr x))))
		       (hash->list full-hash)))
	 (flags (map car opts))
	 (vals (map cdr opts))
	 (comb (map list
		    (map
		     (λ (x) (string-append "--" (symbol->string x)
					   "=")) flags) vals)))
    (string-append "massmine " (string-join (map (λ (x) (string-join x "")) comb) " "))))

;;; Command line input formlet
(define CLI-formlet
  (formlet
   (div 
    (div ((id "task"))
	 "Choose a task:" ,{mm-tasks-formlet . => . task})
    (div ((id "options"))
	 "Auth File:" ,{file-upload-formlet . => . user-auth}
	 "Config File:" ,{file-upload-formlet . => . user-config}
	 "Output File:" ,{input-string . => . user-output}
	 "Count:" ,{input-string . => . user-count}
	 "Date:" ,{input-string . => . user-date}
	 "Duration:" ,{input-string . => . user-dur}
	 "Geo Location:" ,{input-string . => . user-geo}
	 "Language:" ,{input-string . => . user-lang}	 
	 "Query:" ,{input-string . => . user-query}
	 "User:" ,{input-string . => . user-user})
    )
   (hash 'auth (bytes->string/utf-8 user-auth)
	 'config (bytes->string/utf-8 user-config)
	 'output user-output
	 'count user-count 'date user-date 'dur user-dur 'geo user-geo
	 'lang user-lang 'query user-query 'user user-user 'task task)))


;;; Run the user's requested command and capture the output as a string
(define (run-user-command user-command)
  (with-output-to-string (λ () (system user-command))))

;;; main interface template
(define (main-template title body)
  (response/xexpr
   `(html
     (head
      (title ,title)
      (link ((href "style.css") (rel "stylesheet") (type "text/css"))))
     (body
      (a ((href "http://localhost:8000/mmtool")) "Start Over")
      ,@body))))

;;; Confirm the user's request
(define (confirm-user-input input-command request)
  (let* ((cmd (build-mm-command input-command)))
    (main-template
      "MassMine: Your Data Analysis"
     `((p ,(string-append "Command received: " cmd))
	    (p "Results of command execution")
	    (div ((id "output") (style "border:1px solid black;"))
		 (pre ,(run-user-command cmd)))))))



;;; Provide a form for user input
(define (user-input-form request)
  (define (response-generator embed/url)
    (main-template "MassMine: Your Data Analysis"
		   `((h1 "MassMine automated command builder")
		     (form ([action
			     ,(embed/url user-command-handler)])
			   ,@(formlet-display CLI-formlet)
			   (input ([type "submit"]))))))
  (define (user-command-handler request)
    (confirm-user-input
     (formlet-process CLI-formlet request)
     request))
  (send/suspend/dispatch response-generator))

;;; An attempt at a web-server/templates-based approach. 
;;; Template for main page(s)
;; (define (default-template contents)
;;   (include-template "./templates/default.html"))
;; (define (user-input-form request)
;;   (define (response-generator embed/url)
;;     (response/full
;;      200 #"Okay"
;;      (current-seconds) TEXT/HTML-MIME-TYPE
;;      empty
;;      (list (string->bytes/utf-8
;; 	    (default-template
;; 	      `((h1 "MassMine automated command builder")
;; 		(form ([action
;; 			,(embed/url user-command-handler)])
;; 		      ,@(formlet-display CLI-formlet)
;; 		      (input ([type "submit"])))))))))
;;   (define (user-command-handler request)
;;     (confirm-user-input
;;      (formlet-process CLI-formlet request)
;;      request))
;;   (send/suspend/dispatch response-generator))

;;; Start the application (servlet)
(serve/servlet mmgui
	       #:servlet-path "/mmtool"
	       #:servlet-current-directory (current-directory)
	       #:extra-files-paths (list (current-directory)))
