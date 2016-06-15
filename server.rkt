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
 

(define mm-tasks-radio
  (radio-group '("twitter-followers" "twitter-friends"
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

;;; A version with radio buttons (TODO)
(define CLI-formlet
  (formlet
   (div 
    (div ((id "task"))
	 "Choose a task:" ,{mm-tasks-radio . => . task})
    "Options/Flags:" ,{input-string . => . flags-and-options})
   (list task flags-and-options)))


;;; Run the user's requested command and capture the output as a string
(define (run-user-command user-command)
  (with-output-to-string (λ () (system user-command))))

;;; Confirm the user's request
(define (confirm-user-input input-command request)
  (let* ((task (first input-command))
	 (flags (second input-command))
	 (cmd (string-append "massmine --task=" task " " flags)))
    (response/xexpr
     `(html (head (title "MassMine: Your Data Analysis"))
	    (body (p ,(string-append "Command received: " cmd))
		  (p "Results of command execution")
		  (div ((id "output") (style "border:1px solid black;"))
		       (pre ,(run-user-command cmd))))))))



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
