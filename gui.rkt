(require racket/gui)
(require plot)
(require math)
(require json)

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

;;; Data viewer. We try to NOT load a user's entire data set into
;;; memory. However, for the data viewer it is advantageous to
;;; partially read the data set into memory (e.g., to allow for easily
;;; browsing forward and backward through the data set. This parameter
;;; determines how many records (e.g., tweets) to read into
;;; memory. The user could conceivably change this
(define data-viewer-max (make-parameter 50))
;;; The first data-viewer-max lines of the data file are stored in the
;;; following list
(define data-file-preview (make-parameter #f))
;;; The content of the data viewer is stored in memory in this
;;; parameter. It typically holds 1 item from data-file-preview at a
;;; time 
(define data-viewer-content (make-parameter #f))
;;; Current data record to show. This is used with list-ref to show
;;; the current data record
(define data-viewer-idx (make-parameter 0))

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

;;; This function should be called whenever a new data file is set as
;;; the active-data-file. It does things like update the data viewer,
;;; trigger the generation of new analyses, etc.
(define (data-file-update)
  ;; Read in the first n entries from the data file for use in the
  ;; data viewer
  (data-file-preview
   (with-input-from-file (active-data-file)
     (λ ()
       (json-lines->json-array #:head (data-viewer-max)))))
  (data-viewer-content (first (data-file-preview)))
  (data-viewer-idx 0)
  (send (send viewer-field get-editor) erase)
  ;; (send (send viewer-field get-editor) insert (jsexpr->string (data-viewer-content))))
  (send (send viewer-field get-editor)
	insert
	(with-output-to-string (λ () (pretty-print (data-viewer-content)))))
  (send (send viewer-field get-editor)
	    move-position
	    'home)
  (send viewer-counter
	set-label
	(string-append
	 (~r (add1 (data-viewer-idx)) #:min-width 3 #:pad-string "0")
	 "/"
	 (~r (length (data-file-preview)) #:min-width 3 #:pad-string "0"))))

;;; This allows the user to navigate with "previous" and "next"
;;; through their data set using the data viewer. Input parameter i
;;; should be +1 for "next" item, or -1 for "previous" item
(define (update-viewer i)
  (let ([tmp (+ (data-viewer-idx) i)])
    (when (and (active-data-file)
	       (< tmp (length (data-file-preview)))
	       (>= tmp 0))
      (data-viewer-idx tmp)
      (data-viewer-content (list-ref (data-file-preview) tmp))
      (send (send viewer-field get-editor)
	    insert
	    (with-output-to-string (λ () (pretty-print (data-viewer-content)))))
      (send (send viewer-field get-editor)
	    move-position
	    'home)
      (send viewer-counter
	    set-label
	    (string-append
	       (~r (add1 (data-viewer-idx)) #:min-width 3 #:pad-string "0")
	       "/"
	       (~r (length (data-file-preview)) #:min-width 3 #:pad-string "0"))))))

;;; Example data for debugging -----------------------------------------

(define hashtag-data '(("#love" "#hate" "#war") ("15" "10" "8")))
(define users-data '(("@nemo" "@aaron" "@evie") ("24" "10" "7")))

(define render-plot
  (parameterize ([plot-width 300]
		 [plot-height 300]
		 [plot-background "white"])
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
		 (send viewer-panel reparent hidden-frame)
		 (send collection-panel reparent visuals-panel))])
(new button% [parent control-panel]
     [label "Data Viewer"]
      ; Callback procedure for a button click:
     [callback (lambda (button event)
		 (send results-panel reparent hidden-frame)
		 (send settings-panel reparent hidden-frame)
		 (send collection-panel reparent hidden-frame)
		 (send viewer-panel reparent visuals-panel))])
(new button% [parent control-panel]
     [label "Analysis"]
      ; Callback procedure for a button click:
     [callback (lambda (button event)
		 (send collection-panel reparent hidden-frame)
		 (send settings-panel reparent hidden-frame)
		 (send viewer-panel reparent hidden-frame)
		 (send results-panel reparent visuals-panel))])
(new button% [parent control-panel]
     [label "Settings"]
      ; Callback procedure for a button click:
     [callback (lambda (button event)
		 (send collection-panel reparent hidden-frame)
		 (send results-panel reparent hidden-frame)
		 (send viewer-panel reparent hidden-frame)
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

;;; Data viewer panel -------------------------------------------
(define viewer-panel
  (new vertical-panel%
       (parent hidden-frame)))

(define viewer-field
  (new text-field%
       [parent viewer-panel]
       [label #f]
       [init-value ""]
       [style (list 'multiple)]
       [min-height (inexact->exact (round (* .75 (gui-height))))]))
;;; Buttons for navigating through data file preview
(define viewer-controls
  (new horizontal-panel%
       [parent viewer-panel]
       [alignment (list 'center 'center)]))

(new button% [parent viewer-controls]
     [label "Previous"]
      ; Callback procedure for a button click:
     [callback (lambda (button event)
		 (update-viewer -1))])
(define viewer-counter
  (new message%
       [parent viewer-controls]
       [label (string-append
	       (~r 0 #:min-width 3 #:pad-string "0")
	       "/"
	       (~r 0 #:min-width 3 #:pad-string "0"))]))
(new button% [parent viewer-controls]
     [label "Next"]
      ; Callback procedure for a button click:
     [callback (lambda (button event)
		 (update-viewer +1))])

(send (send viewer-field get-editor) insert "Select a data file to view")

;;;  ------------------------------------------------------------


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
	    (GUI-trim-string (path->string choice) 15 #t))
      ;; The following function handles all internal changes related
      ;; to the selection of a new data file
      (data-file-update))))


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
