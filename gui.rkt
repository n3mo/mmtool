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

;;; Control parameters -------------------------------------------------
(define plot? (make-parameter #f))

;;; Toggles
(define (toggle-plot)
  (if (plot?)
      (plot? #f)
      (plot? #t)))

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
			[width 800]
			[height 600]))

;;; Hidden panel for putting things away. Most results windows will
;;; start their lives here
(define hidden-frame
  (new frame%
       [label "Hidden Frame"]))


;;; Main control parameter for analysis options
(define control-panel (new horizontal-panel% [parent main-frame]
			   [alignment '(center top)]))

;;; Panel buttons
(new button% [parent control-panel]
     [label "#Hash Tags"]
      ; Callback procedure for a button click:
     [callback (lambda (button event)
		 (show-hashtags))])
(new button% [parent control-panel]
     [label "@User Mentions"]
      ; Callback procedure for a button click:
     [callback (lambda (button event)
		 (println "@user mentions done!"))])
(new button% [parent control-panel]
     [label "Time Series"]
      ; Callback procedure for a button click:
     [callback (lambda (button event)
		 (toggle-plot)
		 (send plot-canvas on-paint))])

;;; This determines the contents of the tab-panel based on which tab
;;; the user has clicked
(define (update-tab-panel)
  (let* ([tab-number (send tab-panel get-selection)]
	 [tab-name (send tab-panel get-item-label tab-number)])
    (cond [(string=? tab-name "#Hashtags")
	   (send/apply hashtag-table set hashtag-data)
	   (send plot-canvas reparent hidden-frame)
	   (send users-table reparent hidden-frame)
	   (send hashtag-table reparent tab-panel)]
	  [(string=? tab-name "@Users")
	   (send/apply users-table set users-data)
	   (send plot-canvas reparent hidden-frame)
	   (send hashtag-table reparent hidden-frame)
	   (send users-table reparent tab-panel)]
	  [(string=? tab-name "Time Series")
	   (send hashtag-table reparent hidden-frame)
	   (send users-table reparent hidden-frame)
	   (send plot-canvas reparent tab-panel)])))

(define tab-panel (new tab-panel%
                       (parent control-panel)
                       (choices (list "#Hashtags"
                                      "@Users"
                                      "Time Series"))
		       (callback
			(λ (b e)
			  (update-tab-panel)))))

(define plot-canvas
  (new canvas% [parent hidden-frame]
       [paint-callback
	(λ (canvas dc)
	  (if (plot?)
	      (send dc draw-bitmap render-plot 0 0)
	      (send dc clear)))]))

(define hashtag-table
  (new list-box%
       [parent tab-panel]
       [label #f]
       [choices (list )]
       [style (list 'single 'column-headers 'variable-columns)]
       [columns (list "Hashtags" "frequency")]))

(define users-table
  (new list-box%
       [parent hidden-frame]
       [label #f]
       [choices (list )]
       [style (list 'single 'column-headers 'variable-columns)]
       [columns (list "User names" "frequency")]))

;;; Results -------------------------------------------------


;;; Show the main frame
(send main-frame show #t)


;;; END OF GUI CODE -----------------------------------------------------








;;; EXAMPLES IDEAS BELOW

;;; Plot a bitmap to a gui canvase
(define render-plot
  (parameterize ([plot-width 300]
		 [plot-height 300])
      (plot-bitmap (function (distribution-pdf (normal-dist 0 2)) -5 5))))
(define frame (new frame%
                   [label "Example"]
                   [width 300]
                   [height 300]))
(new canvas% [parent frame]
	      [paint-callback
	       (λ (canvas dc)
		 (send dc draw-bitmap render-plot 0 0))])
(send frame show #t)

;;; inserting snips into GUI elements... use editor%
;;; https://docs.racket-lang.org/gui/editor-overview.html?q=send

(define f (new frame% [label "Simple Edit"]
                      [width 200]
                      [height 200]))
(define c (new editor-canvas% [parent f]))
(define t (new text%))
(send c set-editor t)
(send f show #t)
(define pb (new pasteboard%))
(send c set-editor pb)
(define p (plot-snip (function (distribution-pdf (normal-dist 0 2)) -5 5)))

(send pb insert p)


;;; To make a table of data, say from the hashtahs analysis task:
(define frame (new frame% 
                  [label "myTable"]
                  [width 800]
                  [height 600]))

(define table (new list-box%
                 [parent frame]
                 [choices (list )]
                 [label "Test"]
                 [style (list 'single 'column-headers 'variable-columns)]
                 [columns (list "Hashtags" "frequency")]))

(define data '(("#love" "#hate" "#war") ("15" "10" "8")))
(send/apply table set data)
(send frame show #t)
