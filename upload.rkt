#lang racket/base
(require aws
	 racket/format
	 file/gzip
	 racket/file
	 racket/date
	 s3-sync/web
         racket/runtime-path
	 "page.rkt")

(provide upload)

(define-runtime-path plot-rkt "plot.rkt")

(define (upload bucket)
  (define today-name
    (let ([d (seconds->date (current-seconds) #f)])
      (~a (date-year d)
          "-"
          (~r #:min-width 2 #:pad-string "0" #:precision 0 (date-month d))
          "-"
          (~r #:min-width 2 #:pad-string "0" #:precision 0 (date-day d)))))

  (define log-gz (open-output-bytes))
  (call-with-input-file*
   "build-log.txt"
   (lambda (i)
     (gzip-through-ports i log-gz "build-log.txt" (current-seconds))))

  (void
   (put/bytes (~a bucket "/" today-name ".txt")
              (get-output-bytes log-gz)
              "text/plain; charset=utf-8"
              (hash 'x-amz-acl "public-read"
                    'x-amz-storage-class "REDUCED_REDUNDANCY"
                    'Content-Encoding "gzip")))

  (void
   (put/bytes (~a bucket "/" today-name ".png")
              (file->bytes "build-log.png")
              "image/png"
              (hash 'x-amz-storage-class "REDUCED_REDUNDANCY"
                    'x-amz-acl "public-read")))

  (define previous-names
    (let ([l (read (open-input-bytes (get/bytes (~a bucket "/latest"))))])
      (filter (lambda (s) (not (equal? s today-name))) l)))

  (make-page today-name
             (and (pair? previous-names)
                  (car previous-names)))
  (copy-file (build-path "page" "build-plot" (~a today-name ".html"))
             (build-path "page" "build-plot" "index.html")
             #t)
  (copy-file plot-rkt
             (build-path "page" "build-plot" "plot.rkt")
             #t)
  (s3-web-sync "page/build-plot"
               bucket
               ""
               #:upload? #t
               #:reduced-redundancy? #t)

  (void
   (put/bytes (~a bucket "/latest")
              (string->bytes/utf-8
               (~s (cons today-name
                         (if (pair? previous-names)
                             (list (car previous-names))
                             null))))
              "application/octet-stream"
              (hash 'x-amz-acl "public-read"))))
