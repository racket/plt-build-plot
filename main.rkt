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
  (define src-catalog #f)

  (command-line
   #:once-each
   [("--beat") bucket "Record heartbeat at <bucket>"
    (set! beat-bucket bucket)]
   [("-v") "Log `raco setup -v`"
    (set! log-verbose? #t)]
   [("--catalog") catalog "Set the source catalog"
    (set! src-catalog catalog)]
   #:args
   ([bucket #f])
   (build bucket
          #:beat-bucket beat-bucket
          #:log-verbose? log-verbose?
          #:catalog src-catalog)))

(define (system! s)
  (printf "~a\n" s)
  (flush-output)
  (unless (system s)
    (error "failed")))

(define (build bucket
               #:log-verbose? [log-verbose? #f]
               #:work-dir [work-dir (current-directory)]
               #:beat-bucket [beat-bucket #f]
               #:beat-task-name [beat-task-name "build-plot"]
               #:catalog [src-catalog #f])
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
                (if src-catalog (format " SRC_CATALOG=~s" src-catalog) "")
                " > ../build-log.txt 2>&1")))
    
    (read-and-plot (list "build-log.txt") #f #t)
    
    (when bucket
      (upload bucket))
    (flush-output)

    (when beat-bucket
      (beat beat-bucket beat-task-name))))
