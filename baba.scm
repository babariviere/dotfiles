(define-module (baba)
  #:use-module (srfi srfi-1)
  #:export (%channel-root))

(define %channel-root
  (find (lambda (path)
	  (and
	   (file-exists? (string-append path "/.guix-channel"))
	   (file-exists? (string-append path "/baba"))
	   (file-exists? (string-append path "/baba.scm"))))
	%load-path))
