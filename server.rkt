;;; Dependencies
(require racket/runtime-path
	 web-server/servlet
         web-server/servlet-env
	 web-server/formlets
	 web-server/page
	 web-server/dispatch
	 xml)

;;; Runtime path. This is useful for locating bundled web server
;;; items, such as CSS files, JS files, etc.
(define-runtime-path server-path "extras")

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               Parameters
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The current file selected by the user for data
;;; analysis/processing. This acts as a parameter with make-parameter,
;;; but using the local disk (in the user's cache) for managing the
;;; information across threads (and instances). 

(define active-data-file (make-parameter #f))

;; (define (active-data-file [file-name #f])
;;   (if file-name
;;       ;; User has entered a file path for data analysis. Save the
;;       ;; path.
;;       (with-output-to-file (active-file-path)
;; 	(λ () (write file-name))
;; 	#:exists 'replace)
;;       ;; No filename has been supplied. This is a request for the last
;;       ;; active file path
;;       (if (file-exists? (active-file-path))
;; 	  (with-input-from-file (active-file-path)
;; 	    (λ () (read)))
;; 	  ;; No active file has ever existed. Return false
;; 	  #f)))

;;; Is massmine installed and available on the user's path (as
;;; detected by mmtool?). We assume no, but check below. If installed,
;;; this parameter is set to the version number of the installed
;;; massmine executable
(define massmine? (make-parameter #f))

;;; The results of each analysis task are stored in global variables
;;; so that the user can revisit the result after navigating to other
;;; resources in the application. These must be set! by analysis
;;; threads
(define massmine-result #f)
(define hashtags-result #f)
(define user-mentions-result #f)

;;; Each analysis task runs in its own thread. Because nothing
;;; visually happens while an analysis task is running, it would be
;;; useful to provide visual confirmation to the user that a thread is
;;; running. Each task has its own aptly-named thread. Use
;;; (thread-running? <thread>) to see if things are still running. If
;;; so, this should be highlighted on the results page (or somewhere)
(define massmine-thread (make-parameter #f))
(define hashtags-thread (make-parameter #f))
(define user-mentions-thread (make-parameter #f))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               URL Dispatch
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The system can support multiple URLs (disguised as tabs/panels
;; within the "gui")
(define-values (mmtool-dispatch mmtool-url)
    (dispatch-rules
     [("") main-interface]
     [("collect") user-input-form]
     [("analysis") analysis-interface]
     [("results") results-interface]
     [("viewer") viewer-interface]
     ;;[else main-interface]
     ))

(define (mmgui req)
  (mmtool-dispatch req))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            System Checking Routines
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This contains code relevant to providing system checking
;;; functionality (is MassMine installed? are things running
;;; correctly? Can I find the user's data file? etc.

;;; This determines if massmine is correctly installed on the user's
;;; computer and sets the appropriate parameter at startup
(when (find-executable-path "massmine")
    (let ([version (with-output-to-string
		     (λ () (system "massmine --version")))])
      (massmine?
       (car (regexp-match
	     #px"[[:digit:]]{1,2}.[[:digit:]]{1,2}.[[:digit:]]{1,2}"
	     version)))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               Formlets
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; List of tasks in a selection formlet
(define mm-tasks-formlet
	;;; I removed twitter-auth and tumblr-auth from this list
  (select-input '("Choose:" "google-country-trends" "google-trends"
		  "tumblr-blog-info" "tumblr-posts"
		  "tumblr-tag" "twitter-followers"
		  "twitter-friends" "twitter-locations"
		  "twitter-sample" "twitter-search" "twitter-stream"
		  "twitter-trends" "twitter-trends-nohash"
		  "twitter-user" "web-text" "wikipedia-page-links"
		  "wikipedia-search" "wikipedia-text"
		  "wikipedia-views")
		#:attributes '((id "task-selector") (class "pull-right"))))

;;; Analysis tasks formlet
(define analysis-tasks-formlet
  (select-input '("#hashtags" "@user-mentions")))

; File upload formlet
(define file-upload-formlet
  (formlet
   (div ,{(required (file-upload)) . => . a-file})
   a-file))

;;; Command line input formlet
(define CLI-formlet
  (formlet
   (div ((class "col-lg-4"))
	(div ((id "task")) ,{mm-tasks-formlet . => . task})
	(div ((id "options"))
	     ;; "Auth File:" ,{file-upload-formlet . => . user-auth}
	     ;; "Config File:" ,{file-upload-formlet . => . user-config}
	    ; (p "This task requires no input. Submit Query")
	     (ul
	      (li ((id "output") (class "pull-right")) "Output File:     " ,{input-string . => . user-output})
	      (li ((id "count") (class "pull-right")) "Count:     " ,{input-string . => . user-count})
	      (li ((id "date") (class "pull-right")) "Date:     " ,{input-string . => . user-date})
	      (li ((id "duration") (class "pull-right")) "Duration:     " ,{input-string . => . user-dur})
	      (li ((id "location") (class "pull-right")) "Geo Location:     " ,{input-string . => . user-geo})
	      (li ((id "language") (class "pull-right")) "Language:     " ,{input-string . => . user-lang})
	      (li ((id "query") (class "pull-right")) "Query:     " ,{input-string . => . user-query})
	      (li ((id "user") (class "pull-right")) "User:     " ,{input-string . => . user-user}))))
   (hash
    ;; 'auth (bytes->string/utf-8 user-auth)
    ;; 'config (bytes->string/utf-8 user-config)
    'output user-output
    'count user-count 'date user-date 'dur user-dur 'geo user-geo
    'lang user-lang 'query user-query 'user user-user 'task task)))

;;; Analysis main page formlet
(define analysis-formlet
  (formlet
   (div
    (div ((id "task"))
	 "Choose a task:" ,{analysis-tasks-formlet . => . task}))
   (hash 'task task)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           MassMine Command Builder
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; Run the user's requested command and capture the output as a string
(define (run-user-command user-command)
  (with-output-to-string (λ () (system user-command))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         Analysis and Processing Dispatch
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (tmp-hashtags)
;;   '(table (tr (th "Hashtag") (th "Count"))
;; 	  (tr (td "#Vegan") (td "11"))))

;; Each analysis/processing task must be defined here. Each defined
;; procedure must return an X-expression that can be embedded in our
;; main-template
;; (define (process-data-dispatch gui-task)
;;   (if (active-data-file)
;;       (begin
;; 	(cond
;; 	 [(equal? gui-task "#hashtags") (task 'GUI-hashtags)]
;; 	 [(equal? gui-task "@user-mentions") (task 'GUI-user-mentions)])
;; 	(process-data (active-data-file)))
;;       ;; No active data file. Notify the user
;;       `(p "You must select a data file before choosing an analysis task!")))

;;; This method starts a separate thread for each analysis task to
;;; return control immediately to the web app.
(define (process-data-dispatch gui-task)
  (cond
   [(equal? gui-task "#hashtags")
    (task 'GUI-hashtags)
    ;; Remove old results? If someone requests a task, we might
    ;; consider removing any old results so that when they are
    ;; redirected to the results page they don't incorrectly assume
    ;; the old results are their new ones
    ;; (set! hashtags-result #f)
    (hashtags-thread
     (thread (λ () (set! hashtags-result (process-data (active-data-file))))))]
   [(equal? gui-task "@user-mentions")
    (task 'GUI-user-mentions)
    (user-mentions-thread
     (thread (λ () (set! user-mentions-result (process-data (active-data-file))))))])
  ;; Analysis task is off and running. Redirect to the results page,
  ;; which may or may not be ready, depending on how long the analysis
  ;; takes. 
  (results-interface (redirect/get)))

;;; Analysis dispatch. When the user selects an analysis/processing
;;; task, this function determines who to call
;; (define (analysis-dispatch hsh)
;;   (main-template
;;    "MassMine: Your Data Analysis"
;;    `((p ,(string-append "Executed task: " (hash-ref hsh 'task)))
;;      ,(process-data-dispatch (hash-ref hsh 'task)))))
(define (analysis-dispatch hsh)
  (if (active-data-file)
      (process-data-dispatch (hash-ref hsh 'task))
      (main-template
       "MassMine: Your Data Analysis"
       `((p "You must select a data file before choosing an analysis task!")))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        Modals and other x-expression chunks
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Information about the system, and file selector for active file 
(define (system-info)
  `(div ((id "system-info"))
	,(if (massmine?)
	     `(div ((id "installed"))
		   ,(string-append "Your MassMine version: " (massmine?)))
	     `(div ((id "no install"))
		   "No MassMine installation detected!"))
	(h3 "Your working directory is: ")
	(div ,(path->string (current-directory)))
	(h3 "Your active data file is: ")
	(div
	 ,(if (active-data-file)
	      (active-data-file) "<none selected>"))
	(br)))

;;; System settings modal
(define (settings-modal)
  `(div ((class "modal fade")
	 (id "settings-modal")
	 (role "dialog"))
	(div ((class "modal-dialog"))
	     (div ((class "modal-content"))
		  (div ((class "modal-header"))
		       (button ((type "button")
				(class "close")
				(data-dismiss "modal"))
			       times)
		       (h4 ((class "modal-title"))
			   "Settings"))
		  (div ((class "modal-body"))
		       ,(system-info))
		  (div ((class "modal-footer"))
		       (button ((class "btn btn-default")
				(data-dismiss "modal"))
			       "Close"))))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               HTML Templates
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Main interface template
(define (main-template title body)
  (response/xexpr
   #:preamble #"<!DOCTYPE html>"
   `(html
     (head
      (meta ((charset "utf-8")))
      (meta ((http-equiv "X-UA-Compatible") (content "IE=edge")))
      (meta ((name "viewport") (content "width=device-width, initial-scale=1")))
      (title ,title)
      (link ((href "/css/bootstrap.min.css") (rel "stylesheet")))
      (link ((href "/css/simple-sidebar.css") (rel "stylesheet")))
      (link ((href "/font-awesome/css/font-awesome.min.css") (rel "stylesheet")))
      (link ((href "/css/style.css") (rel "stylesheet")))
      (link ((href "/css/jquery.json-viewer.css") (rel "stylesheet")))
      (script ((src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js")))
      (script ((src "/js/jquery.js")))      
      (script ((src "/js/jquery.json-viewer.js"))))
     (body
      ;; full page wrapper--allows side bar menu to shift all content
      ;; move the closing parenthasis for it to above id=page-content-wrapper
      ;; to cover page content instead of shifting it right
      (div ((id "wrapper"))
	   ;; sidebar
	   (div ((id "sidebar-wrapper"))
		(ul ((class "sidebar-nav"))
		    (li ((class "side-top"))
			(h1 "mmtool")
			(a ((href "/")) "Main"))
		    (li (a ((href "/collect")) "Collection"))
		    (li (a ((href "/viewer")) "Data Viewer"))
		    (li (a ((href "/analysis")) "Analysis"))
		    (li (a ((href "/results")) "Results"))))
	   ;; page content wrapper
	   (div ((id "page-content-wrapper"))
		;; (a ((href "#menu-toggle") (id "menu-toggle"))
		;; (i ((class "fa fa-bars") (style "font-size:x-large;"))))
		(div ((class "container-fluid"))
		     (div ((class "row"))
			  (div ((class "col-lg-12"))
			       ;; Sidebar toggle
			       (a ((href "#menu-toggle")
				   (class "btn btn-default")
				   (id "menu-toggle")) "Toggle Menu")
			       ;; Settings modal toggle
			       (button ((type "button")
					(class "btn btn-info btn-md")
					(data-toggle "modal")
					(data-target "#settings-modal"))
				       " "
				       (span ((class "glyphicon glyphicon-cog")))
				       " Settings")
			       ;; And the modal dialog itself
			       ,(settings-modal)
			       ;; all main page content goes here
			       ,@body))))
	   (script ((src "/js/bootstrap.min.js")))
	   (script ((src "/js/custom.js"))))))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               URL Handlers
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This section contains the handlers for each "web page" of the
;;; application

;;; Main interface. The user is greeted with this page on startup at
;;; the root URL
(define (main-interface request)
  (main-template
   "MassMine: Your Data Analysis"
   `((h1 "Welcome to mmtool: The MassMine data collection and analysis tool")
     (p "Please choose an option"))))

;;; Entry point for data analysis/cleaning/exporting/etc. found at "/analysis"
(define (analysis-interface request)
  (define (response-generator embed/url)
    (main-template
     "MassMine: Your Data Analysis"
     `((h1 "Data Processing and Analysis")
       (form ([action
	       ,(embed/url analysis-handler)])
	     ,@(formlet-display analysis-formlet)
	     (input ([type "submit"])))
       (div ((id "data-info"))
	    (h3 "Your active data file is: ")
	    (div ((id "path-view"))
		 ,(if (active-data-file)
		      (active-data-file) "<none selected>"))
	    (br)
	    (form ([action
		    ,(embed/url file-select-handler)])
		  ,@(formlet-display file-upload-formlet)
		  (input ([type "submit"])))))))
  (define (file-select-handler request)
    (active-data-file
     (bytes->string/utf-8
      (formlet-process file-upload-formlet request)))
    (analysis-interface (redirect/get)))
  (define (analysis-handler request)
    (analysis-dispatch (formlet-process analysis-formlet request)))
  (send/suspend/dispatch response-generator))

;;; Results page. If the user has run an analysis, the result(s)
;;; should be available here. For long-running analyses, the user can
;;; be warned to check back here later to see if things have finished
;;; up.
(define (results-interface request)
  (main-template
   "MassMine: Your Data Analysis"
   `((h1 "Data Collection & Analysis Results")
     (p "The results of any analyses can be found below. For large
data sets, it may take time for your results to appear here on the
first run.")
     (div 
      ;; Data collection task
      (h1 "Data Collection  "
	  ,(if (and (massmine-thread) (thread-running? (massmine-thread)))
	       `(a ((href "/results"))
		   (i ((class "fa fa-refresh fa-spin")
		       (style "font-size:30px"))))
	       ""))
      (button ((class "fold_massmine")) "Show/Hide")
      (div ((id "massmine-result"))
	   ,(if massmine-result
		`(pre ,massmine-result)
		"No results found. If you are waiting on a long-running
analysis, refresh this page later."))
      ;; #Hashtags task
      (h1 "#Hashtags  "
	  ,(if (and (hashtags-thread) (thread-running? (hashtags-thread)))
	       `(a ((href "/results"))
		   (i ((class "fa fa-refresh fa-spin")
		       (style "font-size:30px"))))
	       ""))
      (button ((class "fold_hashtags")) "Show/Hide")
      (div ((id "hashtags-result"))
	   ,(if hashtags-result
		hashtags-result
		"No results found. If you are waiting on a long-running
analysis, refresh this page later."))
      ;; @user-mentions task
      (h1 "@User-mentions  "
	  ,(if (and (user-mentions-thread) (thread-running? (user-mentions-thread)))
	       `(a ((href "/results"))
		   (i ((class "fa fa-refresh fa-spin")
		       (style "font-size:30px"))))
	       ""))
      (button ((class "fold_user_mentions")) "Show/Hide")
      (div ((id "user-mentions-result"))
	   ,(if user-mentions-result
		user-mentions-result
		"No results found. If you are waiting on a long-running
analysis, refresh this page later."))))))

;;; This is our JSON data view URL
(define (viewer-interface request)
  (main-template
   "MassMine: Your Data Analysis"
   `((h1 "Data Viewer")
     (p "For optimum browser performance, JSON data beyond 500 entries
is truncated.")
     ,@(if (and (active-data-file)
		(not (string=? (active-data-file) "")))
	  `((pre ((id "json-renderer")))
	    (script
	     ([type "text/javascript"])
	     ,(make-cdata
	       #f
	       #f
	       (string-append
		"var json = "
			(with-input-from-file (active-data-file)
		  (λ () (json-lines->json-array #:head 500)))
		";\n"
		"$('#json-renderer').jsonViewer(json, {collapsed: true});"))))
	  `((p "You must first select a data file"))))))

;;; Confirm the user's request
;; (define (confirm-user-input input-command request)
;;   (let* ((cmd (build-mm-command input-command)))
;;     (main-template
;;      "MassMine: Your Data Analysis"
;;      `((p ,(string-append "Command received: " cmd))
;;        (p "Results of command execution")
;;        (div ((id "results"))
;; 	    (pre ,(run-user-command cmd)))))))
(define (confirm-user-input input-command)
  (let ((cmd (build-mm-command input-command)))
    (massmine-thread
     (thread (λ () (set! massmine-result (run-user-command cmd)))))
    (results-interface (redirect/get))))

;;; The MassMine command builder page at "/collect"
(define (user-input-form request)
  (define (response-generator embed/url)
    (main-template
     "MassMine: Your Data Analysis"
     `((h1 "MassMine automated command builder")
       (form ([action
	       ,(embed/url user-command-handler)])
	     ,@(formlet-display CLI-formlet)
	     (input ([type "submit"]))))))
  (define (user-command-handler request)
    (confirm-user-input
     (formlet-process CLI-formlet request)))
  (send/suspend/dispatch response-generator))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               Server Startup
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is the function you call to start the web-app

;;; Start the application (servlet)
(define (start-gui)
  (serve/servlet mmgui
		 ;;#:servlet-path "/mmtool"
		 #:servlet-path "/"
		 #:servlet-regexp #rx""
		 #:servlet-current-directory (current-directory)
		 #:server-root-path server-path
		 #:extra-files-paths (list (current-directory))))
