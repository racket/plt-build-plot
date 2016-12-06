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
  (define gui? #t)
  (define bitmaps? #f)
  (define fetch? #f)
  (define width 800)
  (define height 600)


  (define srcs
    (command-line
     #:once-each
     [("--batch") "Suppress the GUI viewer"
      (set! gui? #f)]
     [("-o") "Generate \".png\" alongside each <log-file>"
      (set! bitmaps? #t)]
     [("--fetch") "Fetch each <log-file> from build-plot.racket-lang.org"
      (set! fetch? #t)]
     [("--width") w "Set the inital width to <w>"
      (set! width (string->number w))]
     [("--height") h "Set the initial height to <h>"
      (set! height (string->number h))]
     #:args
     (log-file . another-log-file)
     (cons log-file
           another-log-file)))

  (read-and-plot (if fetch? (map fetch srcs) srcs) gui? bitmaps? width height))

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


(define (read-and-plot srcs gui? bitmaps? [w 800] [h 600])
  (define (read-measurements in)
    (let loop ([during ""] [detail ""] [r null])
      (define l (read-line in))
      (cond
       [(eof-object? l) (reverse r)]
       [(regexp-match #rx"^raco setup: version:" l)
        ;; reset
        (loop "" "" null)]
       [(regexp-match #rx"^GC: 0:(?:min|MAJ) @ ([0-9,]+)K[^;]*; free ([-0-9,]+)K[^@]*@ ([0-9]+)$" l)
        => (lambda (m)
             (define mem (string->number (regexp-replace* #rx"," (cadr m) "")))
             (define less-mem (string->number (regexp-replace* #rx"," (caddr m) "")))
             (define time (string->number (cadddr m)))
             (unless (and mem time)
               (error 'build-plot "parse failed: ~s" l))
             (loop during detail
                   (cons (list mem during time less-mem detail)
                         r)))]
       [(regexp-match? #rx"^raco setup: (?:making|running|(?:re-)?rendering)" l)
        (loop l "" r)]
       [else
        (loop during l r)])))

  (define measurementss (for/list ([src (in-list srcs)])
                          (call-with-input-file*
                           src
                           read-measurements)))


  (define all-measurements (apply append measurementss))

  (define (get-max-val measurements)
    (apply max (map car measurements)))

  (define (get-max-time measurements)
    (caddr (last measurements)))

  (define max-val (get-max-val all-measurements))
  (define max-time (apply max (map get-max-time measurementss)))

  ;; ----------------------------------------

  (define (make-graph bm w h measurements)
    (define (x p)
      (* w (/ (caddr p) max-time)))
    (define (y v)
      (- h (* h (/ v (* 1.2 max-val)))))
    (define (y1 p) (y (car p)))
    (define (y2 p) (y (- (car p) (cadddr p))))

    (define dc (send bm make-dc))
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

    (send dc set-pen (make-pen #:color "black"))

    (define p1 (new dc-path%))
    (send p1 move-to 0 (y1 (car measurements)))
    (for ([p (in-list measurements)])
      (send p1 line-to (x p) (y1 p)))
    (send dc draw-path p1 0 0)

    (define p2 (new dc-path%))
    (send p2 move-to 0 (y2 (car measurements)))
    (for ([p (in-list measurements)])
      (send p2 line-to (x p) (y2 p)))
    (send dc draw-path p2 0 0))

  ;; ----------------------------------------

  (when bitmaps?
    (for ([src (in-list srcs)]
          [measurements (in-list measurementss)])
      (define bm (make-bitmap w h #f))
      (make-graph bm w h measurements)
      (send (send bm make-dc)
            draw-text
            (~a "Peak: " (mem-str (get-max-val measurements))
                "   Duration:" (time-str (get-max-time measurements)))
            5 5)
      (send bm save-file (path-replace-suffix src #".png") 'png)))

  ;; ----------------------------------------

  (when gui?
    ((dynamic-require (module-path-index-join
                       '(submod "." gui)
                       (variable-reference->module-path-index
                        (#%variable-reference)))
                      'start-gui)
     srcs measurementss
     max-time max-val make-graph w h)))

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

;; ============================================================

(module* gui #f
  (require racket/gui/base
           racket/class
           racket/path)

  (provide start-gui)

  (define (start-gui srcs measurementss
                     max-time max-val make-graph w h)

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
            (make-graph graph w h measurements))

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

          (when (pair? (cdr srcs))
            (define name (path->string
                          (file-name-from-path (path-replace-suffix (list-ref srcs meas-pos) #""))))
            (define-values (tw th td ta) (send dc get-text-extent name))
            (send dc draw-text name (- w 5 tw) 0)))))

    (void (new graph-canvas% [parent f]))

    (send f show #t)))
