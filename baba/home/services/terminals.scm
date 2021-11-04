(define-module (baba home services terminals)
  #:use-module (gnu home services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu packages terminals)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix import utils) #:select (flatten))

  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (home-foot-configuration
	    home-foot-service-type))

(define foot-config? list?)
(define (serialize-foot-config field-name config)
  (define (serialize-val val)
    (match val
      (#t "yes")
      (#f "no")
      ((? symbol? e) (symbol->string e))
      ((? number? e) (number->string e))
      ((? string? e) e)
      ((lst ...)
       (raise (formatted-message
               (G_ "Sway term should be a non-list value (string, \
boolean, number, symbol, or gexp). Provided term is:\n ~a") lst)))
      (e e)))

  (define (serialize-field field)
    (let ((key (symbol->string (car field)))
  	  (val (serialize-val (cdr field))))
      (list key "=" val "\n")))

  (define (serialize-section fields)
    (append-map serialize-field fields))

  (define (serialize-config config)
    (match config
      (((sections ...) ...)
       (append-map
	(lambda (s)
	  (append (list (format #f "[~a]\n" (car s)))
		  (serialize-section (cadr s))))
	sections))
      (e (raise (formatted-message (G_ "invalid config:\n ~a") e)))))

  #~(apply string-append
	   '#$(serialize-config config)))

(define-configuration home-foot-configuration
  (package
   (package foot)
   "The foot package to use.")
  (config
   (foot-config '())
   "Configuration for foot."))


(define (add-foot-configuration config)
  `(("config/foot/foot.ini"
     ,(mixed-text-file
       "foot.ini"
       (serialize-configuration config home-foot-configuration-fields)))))

(define (add-foot-package config)
  (list (home-foot-configuration-package config)))

(define home-foot-service-type
  (service-type
   (name 'home-foot)
   (extensions
    (list (service-extension
	   home-files-service-type
	   add-foot-configuration)
	  (service-extension
	   home-profile-service-type
	   add-foot-package)))
   (default-value (home-foot-configuration))
   (description "Install and configure the foot terminal emulator")))
