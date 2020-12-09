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

(define (make-page today-name previous-name)
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
	(columns 10
		 #:row? #t
		 (h3 "Racket Build Plots - About and Tools")
		 (p "Each plot depicts memory use during"
		    " the " (tt "raco setup -j 1") " part of installing"
		    " " (tt "main-distribution") 
		    " plus " (tt "main-distribution-test")
		    " in minimal Racket.")
		 (p "The X-direction is CPU time."
		    " The Y-direction for the black line is"
		    " memory use sampled at each GC (before and after"
		    " as separate lines, but you can't usually see distinct"
		    " lines). Each color vertical line corresponds to a"
		    " printout from " (tt "raco setup" ":"))
		 (ul
		  (li blt "orange = " ldquo "making" rdquo " (a collection)")
		  (li blt "blue = " ldquo "running" rdquo " (a document)")
		  (li blt "green = " ldquo "rendering" rdquo " (a document)")
		  (li blt "pink = " ldquo "re-rendering" rdquo " (a document)"))
		 (p "The upward slant in the blue region is not a leak;"
		    " the " ldquo "running" rdquo " phase accumulates"
		    " cross-reference information across documents.")
		 (p "The (smaller) upward slant in the orange region is also not a leak;"
		    " the " ldquo "making" rdquo " phase uses caches"
		    " that increase in size during the build.")
		 (p "Using "  (tt (a href: "plot.rkt" "plot.rkt")) ","
                    " you can generate graphs from the (unzipped) logs,"
                    " show in them in a GUI that provides more detail"
                    " (such as the printouts correcting to the color lines),"
		    " and compare multiple plots."
		    " Use the " (tt "-h") " flag of " (tt "plot.rkt") " for more information.")))

  (make-directory* "page")
  (parameterize ([current-directory "page"])
    (call-with-registered-roots
     render-all)))
