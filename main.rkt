#lang racket/base
(require racket/system
         "plot.rkt"
         "upload.rkt"
         racket/future)

(module+ main
  (require racket/cmdline)
  
  (command-line
   #:args
   (bucket)
   (build bucket)))
  
(define (system! s)
  (printf "~a\n" s)
  (unless (system s)
    (error "failed")))

(define (build bucket [work-dir (current-directory)])
  (define plt-dir (build-path work-dir "plt"))
  (parameterize ([current-directory work-dir])
    (unless (directory-exists? plt-dir)
      (system! "git clone https://github.com/plt/racket plt"))
    
    (parameterize ([current-directory plt-dir])
      (system! "git clean -d -x -f")
      (system! "git pull")
      
      (system! (format "make -j ~a base" (processor-count)))
      
      (system! "racket/bin/raco pkg config --set download-cache-max-files 10240")
      (system! "racket/bin/raco pkg config --set download-cache-max-bytes 671088640")
      
      (system! "env PLTSTDERR=\"debug@GC error\" make CPUS=1 &> ../build-log.txt"))
    
    (read-and-plot (list "build-log.txt") #f #t)
    
    (upload bucket)))
