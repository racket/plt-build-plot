The "plt-build-plot" package implements a service to build the latest
version of Racket, plot the memory use of `raco setup -j 1`, and
upload the results to an S3-hosted web site.

The current directory when running `plt-build-plot/main` will be
populated with a "plt" Git checkout, as well as a "build-log.txt"
and other files.
