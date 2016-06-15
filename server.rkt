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
 

;; (define mm-tasks-formlet
;;   (radio-group '("twitter-followers" "twitter-friends"
;; 		 "twitter-locations" "twitter-sample"
;; 		 "twitter-search" "twitter-stream"
;; 		 "twitter-trends" "twitter-trends-nohash"
;; 		 "twitter-user")))
(define mm-tasks-formlet
  (select-input '("twitter-followers" "twitter-friends"
		  "twitter-locations" "twitter-sample"
		  "twitter-search" "twitter-stream"
		  "twitter-trends" "twitter-trends-nohash"
		  "twitter-user")))

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
	 "Auth File:" ,{input-string . => . user-auth}
	 "Config File:" ,{input-string . => . user-config}
	 "Count:" ,{input-string . => . user-count}
	 "Date:" ,{input-string . => . user-date}
	 "Duration:" ,{input-string . => . user-dur}
	 "Geo Location:" ,{input-string . => . user-geo}
	 "Language:" ,{input-string . => . user-lang}
	 "Output File:" ,{input-string . => . user-output}
	 "Query:" ,{input-string . => . user-query}
	 "User:" ,{input-string . => . user-user})
    )
   (hash 'auth user-auth 'config user-config 'count user-count
	 'date user-date 'dur user-dur 'geo user-geo
	 'lang user-lang 'output user-output 'query user-query
	 'user user-user 'task task)))


;;; Run the user's requested command and capture the output as a string
(define (run-user-command user-command)
  (with-output-to-string (λ () (system user-command))))

;;; Confirm the user's request
(define (confirm-user-input input-command request)
  (let* ((cmd (build-mm-command input-command)))
    (response/xexpr
     `(html (head (title "MassMine: Your Data Analysis"))
	    (body (p ,(string-append "Command received: " cmd))
		  (p "Results of command execution")
		  (div ((id "output") (style "border:1px solid black;"))
		       (pre ,(run-user-command cmd))
		       ))))))



;;; Provide a form for user input
(define (user-input-form request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "MassMine: Your Data Analysis"))
	    (body (p "Please enter a command")
		  (form ([action
			  ,(embed/url user-command-handler)])
			,@(formlet-display CLI-formlet)
			(input ([type "submit"])))))))
  (define (user-command-handler request)
    (confirm-user-input
     (formlet-process CLI-formlet request)
     request))
  (send/suspend/dispatch response-generator))

;;; Start the application (servlet)
(serve/servlet mmgui
	       #:servlet-path "/mmtool")
