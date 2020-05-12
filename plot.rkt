#lang racket/base
(require racket/draw
         racket/class
         racket/cmdline
         racket/list
         racket/format
         racket/port
         net/url)

;; This file needs to stand on its own in a main-distribution Racket.

(provide read-and-plot)

(module+ main
  (define only-major? #f)
  (define gui? #t)
  (define bitmaps? #f)
  (define svgs? #f)
  (define pdfs? #f)
  (define rdcs? #f)
  (define pdf-one-file #f)
  (define dest-dir #f)
  (define fetch? #f)
  (define width 800)
  (define height 600)
  (define linewidth 1)
  (define pen-colors '())

  (define srcs
    (command-line
     #:once-each
     [("--batch") "Suppress the GUI viewer"
      (set! gui? #f)]
     [("-o") "Generate \".png\" alongside each <log-file>"
      (set! bitmaps? #t)]
     [("--svg") "Generate \".svg\" alongside each <log-file>"
      (set! svgs? #t)]
     [("--pdf") "Generate \".pdf\" alongside each <log-file>"
      (set! pdfs? #t)]
     [("--rdc") "Generate \".rdc\" alongside each <log-file>"
      (set! rdcs? #t)]
     [("--one-pdf") file "Generate \".pdf\" <file> with all plots"
      (set! pdf-one-file file)]
     [("--dest") dir "Write files to <dir>"
      (set! dest-dir dir)]
     [("--only-major") "Show only major-GC points"
      (set! only-major? #t)]
     [("--fetch") "Fetch each <log-file> from build-plot.racket-lang.org"
      (set! fetch? #t)]
     [("--width") w "Set the inital width to <w>; default is 800"
      (set! width (string->number w))]
     [("--height") h "Set the initial height to <h>; default is 600"
      (set! height (string->number h))]
     [("--linewidth") n "Set the pen width for the GC line"
      (set! linewidth (string->number n))]
     #:multi
     [("++color") c "Set the (next) pen color for the GC line"
      (set! pen-colors (append pen-colors (list c)))]
     #:args
     (log-file . another-log-file)
     (cons log-file
           another-log-file)))

  (read-and-plot (if fetch? (map fetch srcs) srcs) only-major? gui?
                 bitmaps? svgs? pdfs? rdcs? pdf-one-file
                 width height linewidth pen-colors
                 dest-dir))

(define tmp-dir (build-path (find-system-path 'temp-dir) "plt-build-plot"))

(define (fetch f)
  (unless (directory-exists? tmp-dir)
    (make-directory tmp-dir))
  (define fname (build-path tmp-dir f))
  (unless (file-exists? fname)
    (with-output-to-file fname
      (lambda ()
        (define u (string->url (~a "http://build-plot.racket-lang.org/" f)))
        (call/input-url u get-pure-port (lambda (i) (copy-port i (current-output-port)))))))
  fname)

(define (get-max-val measurements)
  (apply max (map car measurements)))

(define (get-max-time measurements)
  (caddr (last measurements)))

(define (read-and-plot srcs only-major? gui?
                       [bitmaps? #f] [svgs? #f] [pdfs? #f] [rdcs? #f]
                       [pdf-one-file #f]
                       [w 800] [h 600] [linewidth 0] [pen-colors '()]
                       [dest-dir #f])
  (define (match->number s)
    (if s
        (string->number (regexp-replace* #rx"," s ""))
        0))
  (define (read-measurements in)
    (let loop ([during ""] [detail ""] [r null] [peak #f])
      (define l (read-line in))
      (cond
       [(eof-object? l) (cons peak (reverse r))]
       [(regexp-match #rx"^raco setup: version:" l)
        ;; reset
        (loop "" "" null #f)]
       [(regexp-match #rx"^GC: 0:(?:mIn|min|MAJ)[0-9]* @  ?([0-9,]+)K(?:[(]([+-][0-9,]+)K[)])?(?:[[]([+-][0-9,]+)K[]])?[^;]*; free  ?([-0-9,]+)K[^@]*@ ([0-9]+)$" l)
        => (lambda (m)
             (cond
               [(and only-major? (regexp-match? #rx"^GC: 0:min" l))
                (loop during detail r peak)]
               [else
                (define mem (+ (match->number (cadr m))
                               ;; Add space used by JIT code, if reported:
                               (match->number (cadddr m))))
                (define all-mem (+ mem
                                   ;; Add adminisrtraive overhead, if reported:
                                   (match->number (caddr m))))
                (define less-mem (match->number (car (cddddr m))))
                (define time (string->number (cadr (cddddr m))))
                (unless (and mem time)
                  (error 'build-plot "parse failed: ~s" l))
                (loop during detail
                      (cons (list mem during time less-mem detail all-mem)
                            r)
                      peak)]))]
       [(regexp-match? #rx"^raco setup: (?:making|running|(?:re-)?rendering)" l)
        (loop l "" r peak)]
       [(regexp-match #rx"^GC: 0:atexit peak ([0-9,]+)K[(]([+-][0-9,]+)K[)](?:[^[;]*[[][+]([0-9,]+)K)?[^;]*; .*$" l)
        => (lambda (m)
             (loop during l r (+ (match->number (cadr m))
                                 (match->number (caddr m))
                                 (match->number (cadddr m)))))]
       [else
        (loop during l r peak)])))

  (define peak+measurementss (for/list ([src (in-list srcs)])
                               (call-with-input-file*
                                src
                                read-measurements)))


  (define peaks (map car peak+measurementss))
  (define measurementss (map cdr peak+measurementss))
  
  (define all-measurements (apply append measurementss))

  (define max-val (max (get-max-val all-measurements)
                       (apply max (for/list ([peak (in-list peaks)])
                                    (or peak 0)))))
  (define max-time (apply max (map get-max-time measurementss)))

  ;; ----------------------------------------

  (define (make-graph dc w h color peak measurements)
    (define (x p)
      (* w (/ (caddr p) max-time)))
    (define (y v)
      (- h (* h (/ v (* 1.2 max-val)))))
    (define (y1 p) (y (car p)))
    (define (y2 p) (y (- (car p) (cadddr p))))
    (define (y5 p) (y (list-ref p 5)))

    (send dc set-smoothing 'smoothed)
    (send dc set-brush (make-brush #:style 'transparent))

    (for/fold ([during ""]) ([p (in-list measurements)])
      (define new-during (cadr p))
      (unless (equal? during new-during)
        (send dc set-pen (make-pen #:color (cond
                                            [(regexp-match? #"running" during) "cyan"]
                                            [(regexp-match? #"re-rendering" during) "pink"]
                                            [(regexp-match? #"rendering" during) "green"]
                                            [else "orange"])))
        (send dc draw-line (x p) 0 (x p) h))
      new-during)

    (send dc set-pen (make-pen #:color "darkgray" #:width linewidth))
    
    (define p5 (new dc-path%))
    (send p5 move-to 0 (y5 (car measurements)))
    (for ([p (in-list measurements)])
      (send p5 line-to (x p) (y5 p)))
    (send dc draw-path p5 0 0)

    (send dc set-pen (make-pen #:color color #:width linewidth))

    (define p1 (new dc-path%))
    (send p1 move-to 0 (y1 (car measurements)))
    (for ([p (in-list measurements)])
      (send p1 line-to (x p) (y1 p)))
    (send dc draw-path p1 0 0)

    (define p2 (new dc-path%))
    (send p2 move-to 0 (y2 (car measurements)))
    (for ([p (in-list measurements)])
      (send p2 line-to (x p) (y2 p)))
    (send dc draw-path p2 0 0)

    (when peak
      (send dc set-pen (make-pen #:color "red"))
      (send dc draw-line 0 (y peak) w (y peak))))

  ;; ----------------------------------------

  (define (draw-all mode [one-file #f])
    (define one-dc #f)
    (for ([src (in-list srcs)]
          [peak (in-list peaks)]
          [measurements (in-list measurementss)]
          [i (in-naturals)])
      (define dest-src (if dest-dir
                           (if one-file
                               (if (relative-path? one-file)
                                   (build-path dest-dir one-file)
                                   one-file)
                               (let-values ([(base name dir?) (split-path src)])
                                 (build-path dest-dir name)))
                           (if one-file
                               one-file
                               src)))
      (define bm (and (eq? mode 'png) (make-bitmap w h #f)))
      (define dc (case mode
                   [(png) (send bm make-dc)]
                   [(rdc) (new record-dc%
                               [width w]
                               [height h])]
                   [(svg pdf)
                    (define dc
                      (or one-dc
                          (case mode
                            [(svg)
                             (new svg-dc%
                                  [width w]
                                  [height h]
                                  [output (path-replace-suffix dest-src #".svg")])]
                            [(pdf)
                             (let ([setup (new ps-setup%)])
                               (send setup set-scaling 1.0 1.0)
                               (parameterize ([current-ps-setup setup])
                                 (new pdf-dc%
                                      [as-eps #f]
                                      [interactive #f]
                                      [width w]
                                      [height h]
                                      [output (if one-file
                                                  dest-src
                                                  (path-replace-suffix dest-src #".pdf"))])))])))
                    (unless one-dc
                      (send dc start-doc "plot")
                      (when one-file
                        (set! one-dc dc)))
                    (send dc start-page)
                    dc]))
      (when one-dc
        (define-values (sw sh) (send dc get-size))
        (define s (min (/ sw w) (/ sh h)))
        (send dc set-scale s s))
      (define color (select i pen-colors))
      (make-graph dc w h color peak measurements)
      (send dc
            draw-text
            (~a (if peak
                    (~a "Peak: " (mem-str peak) "   ")
                    "")
                "Peak allocated: " (mem-str (get-max-val measurements))
                "   Duration:" (time-str (get-max-time measurements)))
            5 5)
      (when one-dc
        (define (draw-string str dy)
          (define-values (tw th td ta) (send dc get-text-extent str))
          (send dc draw-text str (- w 5 tw) dy)
          (+ dy th))
        (define-values (base name dir?) (split-path src))
        (let ([dy (draw-string (path->string (path-replace-suffix name #"")) 5)])
          (void)))
      (case mode
        [(png) (send bm save-file (path-replace-suffix dest-src #".png") 'png)]
        [(rdc) (call-with-output-file*
                (path-replace-suffix dest-src #".rdc")
                #:exists 'truncate/replace
                (lambda (o)
                  (writeln (send dc get-recorded-datum) o)))]
        [(svg pdf)
         (send dc end-page)
         (unless one-dc
           (send dc end-doc))]))
    (when one-dc
      (send one-dc end-doc)))

  (when bitmaps? (draw-all 'png))
  (when svgs? (draw-all 'svg))
  (when pdfs? (draw-all 'pdf))
  (when rdcs? (draw-all 'rdc))
  (when pdf-one-file (draw-all 'pdf pdf-one-file))

  ;; ----------------------------------------

  (when gui?
    ((dynamic-require (module-path-index-join
                       '(submod "." gui)
                       (variable-reference->module-path-index
                        (#%variable-reference)))
                      'start-gui)
     srcs peaks measurementss
     max-time max-val make-graph w h pen-colors)))

;; ----------------------------------------

(define (mem-str mem)
  (~a (comma-ize (~r mem #:precision 0))
      "K"))

(define (comma-ize s)
  (define l (string-length s))
  (if (l . > . 3)
      (string-append (comma-ize (substring s 0 (- l 3)))
                     ","
                     (substring s (- l 3)))
      s))

(define (time-str secs)
  (time-ize (floor secs)))

(define (time-ize n)
  (~a (~r (quotient n (* 60 60 1000))
          #:min-width 1
          #:pad-string "0")
      ":"
      (~r (modulo (quotient n (* 60 1000)) 60)
          #:min-width 2
          #:pad-string "0")
      ":"
      (~r (/ (modulo n (* 60 1000)) 1000.0)
          #:precision '(= 2)
          #:min-width 5
          #:pad-string "0")))

(define (select i pen-colors)
  (cond
    [(i . < . (length pen-colors)) (list-ref pen-colors i)]
    [(null? pen-colors) "black"]
    [else (last pen-colors)]))

;; ============================================================

(module* gui #f
  (require racket/gui/base
           racket/class
           racket/path)

  (provide start-gui)

  (define (start-gui srcs peaks measurementss
                     max-time max-val make-graph w h pen-colors)

    (define f (new frame%
                   [label "Memory"]
                   [width w]
                   [height h]))

    (define graph-canvas%
      (class canvas%
        (super-new)
        (inherit get-dc
                 get-client-size
                 refresh)

        (define meas-pos 0)
        (define peak (list-ref peaks meas-pos))
        (define measurements (list-ref measurementss meas-pos))

        (define mouse-x #f)
        (define mouse-y #f)
        (define mouse-x1 #f)
        (define mouse-y1 #f)
        (define mouse-x2 #f)
        (define mouse-y2 #f)

        (define/override (on-event e)
          (define-values (w h) (get-client-size))
          (define new-x (min (max 0 (send e get-x)) w))
          (define new-y (min (max 0 (send e get-y)) h))
          (unless (and (eqv? mouse-x new-x)
                       (eqv? mouse-y new-y))
            (set! mouse-x new-x)
            (set! mouse-y new-y)
            (refresh))
          (cond
           [(send e button-down? 'left)
            (set! mouse-x1 new-x)
            (set! mouse-x2 new-x)
            (set! mouse-y1 new-y)
            (set! mouse-y2 new-y)
            (refresh)]
           [(and (send e button-up? 'left)
                 (eqv? mouse-x1 mouse-x2)
                 (eqv? mouse-y1 mouse-y2))
            (set! mouse-x1 #f)
            (set! mouse-y1 #f)
            (set! mouse-x2 #f)
            (set! mouse-y2 #f)
            (refresh)]
           [(or (send e button-up? 'left)
                (and (send e get-left-down)
                     mouse-x2))
            (unless (and (eqv? mouse-x2 new-x)
                         (eqv? mouse-y2 new-y))
              (set! mouse-x2 new-x)
              (set! mouse-y2 new-y)
              (refresh))]))

        (define/override (on-char e)
          (define (adj d)
            (define new-meas-pos (modulo (+ meas-pos d (length srcs))
                                         (length srcs)))
            (unless (= meas-pos new-meas-pos)
              (set! meas-pos new-meas-pos)
              (set! peak (list-ref peaks meas-pos))
              (set! measurements (list-ref measurementss meas-pos))
              (set! graph #f)
              (refresh)))
          (case (send e get-key-code)
            [(left) (adj -1)]
            [(right) (adj +1)]))

        (define graph-w 0)
        (define graph-h 0)
        (define graph #f)

        (define/override (on-paint)
          (define dc (get-dc))
          (define-values (w h) (get-client-size))
          (define (x p)
            (* w (/ (caddr p) max-time)))

          (unless (and graph
                       (= w graph-w)
                       (= h graph-h))
            (set! graph-w w)
            (set! graph-h h)
            (set! graph (make-bitmap w h))
            (make-graph (send graph make-dc) w h (select meas-pos pen-colors) peak measurements))

          (send dc draw-bitmap graph 0 0)

          (define mouse-p
            (and mouse-x
                 (or (for/first ([p (in-list measurements)]
                                 #:when ((x p) . >= . mouse-x))
                       p)
                     #f)))

          (define mouse-during (or (and mouse-p (cadr mouse-p))
                                   ""))
          (define mouse-detail (or (and mouse-p (list-ref mouse-p 4))
                                   ""))

          (define mouse-mem (or (and mouse-p (list-ref mouse-p 0))
                                0))

          (define (max-mem-of x0 x1)
            (for/fold ([m 0])
                      ([p (in-list measurements)]
                       #:when ((x p) . >= . x0)
                       #:break ((x p) . > . x1))
              (max m (car p))))


          (define (mem-of dh)
            (mem-str (* max-val 1.2 (/ dh h))))

          (define (time-of dw)
            (time-str (* max-time (/ dw w))))

          (when mouse-x1
            (define x (min mouse-x1 mouse-x2))
            (define y (min mouse-y1 mouse-y2))
            (define dw (abs (- mouse-x2 mouse-x1)))
            (define dh (abs (- mouse-y2 mouse-y1)))
            (send dc set-pen (make-pen #:color "blue" #:width 2))
            (send dc draw-line x 0 x h)
            (send dc draw-line (+ x dw) 0 (+ x dw) h)
            (send dc set-pen (make-pen #:color "blue"))
            (send dc set-brush (make-brush #:color (let ([c (make-object color% "blue")])
                                                     (make-color (send c red)
                                                                 (send c green)
                                                                 (send c blue)
                                                                 0.5))))
            (send dc draw-rectangle x y dw dh)
            (send dc set-font (make-font #:size 12 #:size-in-pixels? #t #:weight 'bold))
            (define desc (~a (mem-str (max-mem-of x (+ x dw)))
                             "    "
                             (mem-of dh)
                             "    "
                             (time-of dw)))
            (send dc set-text-foreground "white")
            (send dc draw-text desc (+ x 2) (+ y 2))
            (send dc set-text-foreground "black")
            (send dc draw-text desc (+ x 3) (+ y 3)))

          (send dc set-font (make-font #:size 12 #:size-in-pixels? #t))
          (send dc draw-text mouse-during 5 0)
          (send dc draw-text mouse-detail 5 14)
          (when mouse-x
            (send dc draw-text (~a (mem-str mouse-mem)
                                   "   "
                                   (time-of mouse-x))
                  5 30)
            (send dc draw-text (mem-of (- h mouse-y))
                  (+ 5 mouse-x) (+ -10 mouse-y)))

          (when mouse-x
            (define p (send dc get-pen))
            (send dc set-pen "red" 1 'solid)
            (send dc draw-line mouse-x 0 mouse-x h)
            (send dc set-pen p)
            (void))

          (define (draw-string str dy)
            (define-values (tw th td ta) (send dc get-text-extent str))
            (send dc draw-text str (- w 5 tw) dy)
            (+ dy th))

          (let* ([dy 0]
                 [dy (if (pair? (cdr srcs))
                         (draw-string (path->string
                                       (file-name-from-path (path-replace-suffix (list-ref srcs meas-pos) #"")))
                                      dy)
                         dy)]
                 [dy (if peak
                         (draw-string (~a "Peak: " (mem-str peak))
                                      dy)
                         dy)]
                 [dy (draw-string (~a "Peak allocated: " (mem-str (get-max-val measurements)))
                                  dy)]
                 [dy (draw-string (~a "Duration:" (time-str (get-max-time measurements)))
                                  dy)])
            (void dy)))))

    (void (new graph-canvas% [parent f]))

    (send f show #t)))
