#lang racket/base
(require racket/system
         "plot.rkt"
         "upload.rkt"
         racket/future
         plt-service-monitor/beat)

(provide build)

(module+ main
  (require racket/cmdline)
  
  (define beat-bucket #f)
  (define log-verbose? #f)

  (command-line
   #:once-each
   [("--beat") bucket "Record heartbeat at <bucket>"
    (set! beat-bucket bucket)]
   [("-v") "Log `raco setup -v`"
    (set! log-verbose? #t)]
   #:args
   ([bucket #f])
   (build bucket
          #:beat-bucket beat-bucket
          #:log-verbose? log-verbose?)))

(define (system! s)
  (printf "~a\n" s)
  (flush-output)
  (unless (system s)
    (error "failed")))

(define (build bucket
               #:log-verbose? [log-verbose? #f]
               #:work-dir [work-dir (current-directory)]
               #:beat-bucket [beat-bucket #f]
               #:beat-task-name [beat-task-name "build-plot"])
  (define plt-dir (build-path work-dir "plt"))
  (parameterize ([current-directory work-dir])
    (unless (directory-exists? plt-dir)
      (system! "git clone https://github.com/racket/racket plt"))
    
    (parameterize ([current-directory plt-dir])
      (system! "git clean -d -x -f")
      (system! "git pull")
      
      (system! (format "make -j ~a base" (processor-count)))
      
      (system! "racket/bin/raco pkg config --set download-cache-max-files 10240")
      (system! "racket/bin/raco pkg config --set download-cache-max-bytes 671088640")
      
      (system! (string-append
                "env PLTSTDERR=\"debug@GC error\" make CPUS=1"
                (if log-verbose? " PLT_SETUP_OPTIONS=-v" "")
                " > ../build-log.txt 2>&1")))
    
    (read-and-plot (list "build-log.txt") #f #t)
    
    (when bucket
      (upload bucket))
    (flush-output)

    (when beat-bucket
      (beat beat-bucket beat-task-name))))
