(require racket/gui)
(require plot)
(require math)

;;; Useful tricks -------------------------------------------------
;;; Use (send plot-canvas show #f) to hide a window, such as plot-canvas or
;;; (send plot-canvas show #t) to reveal it again. You can draw to or
;;; change the canvas whether it is visible or not.

;;; After creating a plot (such as render-plot below) and displaying
;;; it in a canvas, you can then change render-plot to something
;;; else. However, for the change to manifest in the canvas, you need
;;; to trigger the paint-callback procedure for that canvas. Many
;;; things, such as window resizing trigger this, but you can force it
;;; to happen immediately with (send plot-canvas on-paint)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               Parameters
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Overall window sizing
(define gui-width (make-parameter 800))
(define gui-height (make-parameter 600))

;;; The current file selected by the user for data
;;; analysis/processing.
(define active-data-file (make-parameter #f))
;;; The current active data file's data type (i.e., is the data from
;;; twitter-stream, wikipedia-text, etc.)
(define active-data-type (make-parameter #f))

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
(define time-series-result #f)

;;; Each analysis task runs in its own thread. Because nothing
;;; visually happens while an analysis task is running, it would be
;;; useful to provide visual confirmation to the user that a thread is
;;; running. Each task has its own aptly-named thread. Use
;;; (thread-running? <thread>) to see if things are still running. If
;;; so, this should be highlighted on the results page (or somewhere)
(define massmine-thread (make-parameter #f))
(define hashtags-thread (make-parameter #f))
(define user-mentions-thread (make-parameter #f))
(define time-series-thread (make-parameter #f))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               Helper Functions
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; String shortener for GUI messages
(define (GUI-trim-string str n [file? #f])
  (let ([len (string-length str)]
	[chars (string->list str)]
	[tokens (round (/ (- n 3) 2))])
    (if file?
	;; For files, truncate the start of the file path
	(list->string
	 (append '(#\. #\. #\.)
		 (reverse (take (reverse chars) (- n 3)))))
	;; For directories, preserve the beginning and end of paths
	(cond [(<= len n) str]
	      [(odd? n)
	       (list->string
		(append (take chars tokens)
			'(#\. #\. #\.)
			(reverse (take (reverse chars) tokens))))]
	      [else
	       (list->string
		(append (take chars (sub1 tokens))
			'(#\. #\. #\.)
			(reverse (take (reverse chars) tokens))))]))))


;;; Example data for debugging -----------------------------------------

(define hashtag-data '(("#love" "#hate" "#war") ("15" "10" "8")))
(define users-data '(("@nemo" "@aaron" "@evie") ("24" "10" "7")))

(define render-plot
  (parameterize ([plot-width 300]
		 [plot-height 300])
      (plot-bitmap (function (distribution-pdf (normal-dist 0 2)) -5 5))))


;;; Default frame -------------------------------------------------
;;; Everything that the user is seeing goes here
(define main-frame (new frame%
			[label "MassMine Analytics"]
			[width (gui-width)]
			[height (gui-height)]))

;;; Hidden panel for putting things away. Most results windows will
;;; start their lives here
(define hidden-frame
  (new frame%
       [label "Hidden Frame"]))

;;; Main panel that holds everything. This controls the overall
;;; layout, such that the controls are on the left, for example, and
;;; the visualizations are on the right.
(define main-panel
  (new horizontal-panel% [parent main-frame]
       [alignment '(center top)]))

;;; Main control parameter for button interface
(define control-panel (new horizontal-panel% [parent main-panel]
			   [alignment '(center top)]
			   [min-width (round (/ (gui-width) 2))]))

;;; Things that dynamically change with user interaction appear over
;;; here
(define visuals-panel
  (new panel%
       [parent main-panel]
       [style (list 'auto-vscroll 'auto-hscroll)]
       [alignment '(center top)]
       [min-width (round (/ (gui-width) 2))]))

;;; Panel buttons
(new button% [parent control-panel]
     [label "Collection"]
      ; Callback procedure for a button click:
     [callback (lambda (button event)
		 (send results-panel reparent hidden-frame)
		 (send settings-panel reparent hidden-frame)
		 (send collection-panel reparent visuals-panel))])
(new button% [parent control-panel]
     [label "Data Viewer"]
      ; Callback procedure for a button click:
     [callback (lambda (button event)
		 (send results-panel reparent hidden-frame)
		 (send settings-panel reparent hidden-frame)
		 (send collection-panel reparent hidden-frame)
		 (send collection-panel reparent visuals-panel))])
(new button% [parent control-panel]
     [label "Analysis"]
      ; Callback procedure for a button click:
     [callback (lambda (button event)
		 (send collection-panel reparent hidden-frame)
		 (send settings-panel reparent hidden-frame)
		 (send results-panel reparent visuals-panel))])
(new button% [parent control-panel]
     [label "Settings"]
      ; Callback procedure for a button click:
     [callback (lambda (button event)
		 (send collection-panel reparent hidden-frame)
		 (send results-panel reparent hidden-frame)
		 (send settings-panel reparent visuals-panel))])

;;; This determines the contents of the results-panel based on which tab
;;; the user has clicked
(define (update-results-panel)
  (let* ([tab-number (send results-panel get-selection)]
	 [tab-name (send results-panel get-item-label tab-number)])
    (cond [(string=? tab-name "#Hashtags")
	   (send/apply hashtag-table set hashtag-data)
	   (send plot-canvas reparent hidden-frame)
	   (send users-table reparent hidden-frame)
	   (send hashtag-table reparent results-panel)]
	  [(string=? tab-name "@Users")
	   (send/apply users-table set users-data)
	   (send plot-canvas reparent hidden-frame)
	   (send hashtag-table reparent hidden-frame)
	   (send users-table reparent results-panel)]
	  [(string=? tab-name "Time Series")
	   (send hashtag-table reparent hidden-frame)
	   (send users-table reparent hidden-frame)
	   (send plot-canvas reparent results-panel)])))

(define results-panel
  (new tab-panel%
       (parent hidden-frame)       
       (choices (list "#Hashtags"
		      "@Users"
		      "Time Series"))
       (callback
	(λ (b e)
	  (update-results-panel)))))

(define collection-panel
  (new panel%
       (parent hidden-frame)))

;;; App settings panel -------------------------------------------
(define (set-working-directory)
  (let ([choice (get-directory "Choose working directory"
			       main-frame
			       (current-directory))])
    (when choice
      (current-directory choice)
      (send CWD-message set-label
	    (GUI-trim-string (path->string choice) 15)))))

(define (set-active-data-file)
  (let ([choice (get-file "Choose a data file"
			       main-frame
			       (current-directory))])
    (when choice
      (active-data-file choice)
      (send ADF-message set-label
	    (GUI-trim-string (path->string choice) 15 #t)))))


(define settings-panel
  (new vertical-panel%
       (parent hidden-frame)))

;;; --------------------------------------------------------------
(define CWD-info
  (new horizontal-panel%
       [parent settings-panel]))

;;; Buttons for settings panel
(new button% [parent CWD-info]
     [label "Set"]
      ; Callback procedure for a button click:
     [callback (lambda (button event)
		 (set-working-directory))])
(define CWD-message
  (new message%
       [parent CWD-info]
       [auto-resize #t]
       [label (GUI-trim-string (path->string (current-directory)) 15)]))
;;; --------------------------------------------------------------

;;; --------------------------------------------------------------
(define ADF-info
  (new horizontal-panel%
       [parent settings-panel]))

;;; Buttons for settings panel
(new button% [parent ADF-info]
     [label "Set"]
      ; Callback procedure for a button click:
     [callback (lambda (button event)
		 (set-active-data-file))])
(define ADF-message
  (new message%
       [parent ADF-info]
       [auto-resize #t]
       [label (if (active-data-file)
		  (path->string (active-data-file))
		  "none selected")]))
;;; --------------------------------------------------------------

(define plot-canvas
  (new canvas%
       [parent hidden-frame]       
       [paint-callback
	(λ (canvas dc)
	  (send dc draw-bitmap render-plot 0 0))]))

(define hashtag-table
  (new list-box%
       [parent results-panel]
       [label #f]
       [choices (list )]
       [style (list 'multiple 'column-headers 'variable-columns)]
       [columns (list "Hashtag" "frequency")]))

(define users-table
  (new list-box%
       [parent hidden-frame]
       [label #f]
       [choices (list )]
       [style (list 'multiple 'column-headers 'variable-columns)]
       [columns (list "User" "frequency")]))

;;; Results -------------------------------------------------


;;; Show the main frame
(send main-frame show #t)


;;; END OF GUI CODE -----------------------------------------------------








;;; EXAMPLES IDEAS BELOW

;;; Plot a bitmap to a gui canvase
;; (define render-plot
;;   (parameterize ([plot-width 300]
;; 		 [plot-height 300])
;;       (plot-bitmap (function (distribution-pdf (normal-dist 0 2)) -5 5))))
;; (define frame (new frame%
;;                    [label "Example"]
;;                    [width 300]
;;                    [height 300]))
;; (new canvas% [parent frame]
;; 	      [paint-callback
;; 	       (λ (canvas dc)
;; 		 (send dc draw-bitmap render-plot 0 0))])
;; (send frame show #t)

;; ;;; inserting snips into GUI elements... use editor%
;; ;;; https://docs.racket-lang.org/gui/editor-overview.html?q=send

;; (define f (new frame% [label "Simple Edit"]
;;                       [width 200]
;;                       [height 200]))
;; (define c (new editor-canvas% [parent f]))
;; (define t (new text%))
;; (send c set-editor t)
;; (send f show #t)
;; (define pb (new pasteboard%))
;; (send c set-editor pb)
;; (define p (plot-snip (function (distribution-pdf (normal-dist 0 2)) -5 5)))

;; (send pb insert p)


;; ;;; To make a table of data, say from the hashtahs analysis task:
;; (define frame (new frame% 
;;                   [label "myTable"]
;;                   [width 800]
;;                   [height 600]))

;; (define table (new list-box%
;;                  [parent frame]
;;                  [choices (list )]
;;                  [label "Test"]
;;                  [style (list 'single 'column-headers 'variable-columns)]
;;                  [columns (list "Hashtags" "frequency")]))

;; (define data '(("#love" "#hate" "#war") ("15" "10" "8")))
;; (send/apply table set data)
;; (send frame show #t)
