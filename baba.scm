(define-module (baba)
  #:use-module (srfi srfi-1)
  #:export (%channel-root etc-file))

(define %channel-root
  (canonicalize-path
   (find (lambda (path)
	   (and
	    (file-exists? (string-append path "/.guix-channel"))
	    (file-exists? (string-append path "/baba"))
	    (file-exists? (string-append path "/baba.scm"))))
	 %load-path)))

(define (etc-file path)
  (string-append %channel-root "/etc" path))
