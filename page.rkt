#lang racket/base
(require plt-web
	 scribble/html
	 (only-in plt-web/style columns)
	 racket/format
	 racket/file)

(provide make-page)

(define www (site "www"
		  #:url "https://racket-lang.org/"
		  #:generate? #f))
(define build-plot (site "build-plot"
			 #:url "https://build-plot.racket-lang.org/"
			 #:share-from www))

(define (make-page today-name previous-name
                   #:available-plots [available-plots '()])
  (page #:site build-plot
	#:file (~a today-name ".html")
	#:title "Racket Build Plot"
	(columns 10
		 #:row? #t
		 (h3 today-name)
		 (img src: (~a today-name ".png"))
		 (div "Log: "
		      (a href: (~a today-name ".txt")
			 (~a today-name ".txt"))
		      (for/list ([i 8]) nbsp)
		      (a href: "about.html"
			 "About and Tools"))
		 (div (if previous-name
			  (a href: (~a previous-name ".html")
			     "back")
			  nbsp))))

  (define blt (list nbsp bull nbsp))
  (page #:site build-plot
	#:file "about.html"
	#:title "Racket Build Plot - About and Tools"
	(apply columns 10
               #:row? #t
               (h3 "Racket Build Plots - About and Tools")
               (p "Each plot depicts memory use during"
                  " the " (tt "raco setup -j 1") " part of installing"
                  " " (tt "main-distribution") 
                  " plus " (tt "main-distribution-test")
                  " in minimal Racket.")
               (p "The X-direction is CPU time."
                  " Each color vertical line corresponds to a"
                  " printout from " (tt "raco setup" ":"))
               (ul
                (li blt "orange = " ldquo "making" rdquo " (a collection)")
                (li blt "blue = " ldquo "running" rdquo " (a document)")
                (li blt "green = " ldquo "rendering" rdquo " (a document)")
                (li blt "pink = " ldquo "re-rendering" rdquo " (a document)"))
               (p "The Y-direction for the black line is"
                  " memory use sampled at each garbage collection; before and after"
                  " are separate lines, but you can't usually see distinct"
                  " lines if minor collections are rendered, which is the"
                  " default for Racket BC. The gray line just above the"
                  " black line is total memory with collector overhead"
                  " just before a collection, so it's a truer measure of total memory use."
                  " The red line (parallel to the X-axis) shows peak memory"
                  " use including collector overhead; that peeak can occur in between"
                  " collections, but it should be fairly close to the gray line's peak.")
               (p "By default, a garbage collection is forced at the start"
                  " of compiling each collection and running or rendering each document."
                  " In that configuration, the black line will tend to"
                  " return a live-memory baseline value frequently, which reduces"
                  " overall memory use at the expense of some time."
                  " A slight upward slant  for that baseline in the orange region"
                  " is not necessarily a leak; the " ldquo "making" rdquo " phase uses caches"
                  " that increase in size (toward some limit) during the build."
                  " An upward slant in the blue region is also not a leak;"
                  " the " ldquo "running" rdquo " phase accumulates"
                  " cross-reference information across documents.")
               (p "Using "  (tt (a href: "plot.rkt" "plot.rkt")) ","
                  " you can generate graphs from the (unzipped) logs,"
                  " show in them in a GUI that provides more detail"
                  " (such as the printouts correcting to the color lines),"
                  " and compare multiple plots."
                  " Use the " (tt "-h") " flag of " (tt "plot.rkt") " for more information.")
               (if (null? available-plots)
                   null
                   (list
                    (h3 "Available Plots")
                    (apply ul
                           (for/list ([ap (in-list available-plots)])
                             (li blt (a href: (cadr ap) (car ap)))))))))

  (make-directory* "page")
  (parameterize ([current-directory "page"])
    (call-with-registered-roots
     render-all)))

(module+ main
  (require racket/cmdline)
  
  (define available-plots '())
  
  (command-line
   #:once-each
   [("--available") file "Read list of `(<desc> <url>)` from <file>"
    (set! available-plots (call-with-input-file* file read))]
   #:args
   ()
   (void))
  
  (make-page "today" "yesterday"
             #:available-plots available-plots))
