(define-module (baba home services shells)
  #:use-module (gnu home services)
  #:use-module (gnu packages shells)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:replace (home-fish-service-type
             home-fish-configuration
             home-fish-extension))

;;;
;;; Fish.
;;;

(define (serialize-fish-aliases field-name val)
  #~(string-append
     #$@(map (match-lambda
               ((key . value)
                #~(string-append "alias " #$key " \"" #$value "\"\n"))
               (_ ""))
             val)))

(define (serialize-fish-abbreviations field-name val)
  #~(string-append
     #$@(map (match-lambda
               ((key . value)
                #~(string-append "abbr --add " #$key " " #$value "\n"))
               (_ ""))
             val)))

(define (serialize-fish-env-vars field-name val)
  #~(string-append
     #$@(map (match-lambda
               ((key . #f)
                "")
               ((key . #t)
                #~(string-append "set " #$key "\n"))
               ((key . value)
                #~(string-append "set " #$key " "  #$value "\n")))
             val)))

(define-configuration home-fish-configuration
  (package
    (package fish)
    "The Fish package to use.")
  (config
   (text-config '())
   "List of file-like objects, which will be added to
@file{$XDG_CONFIG_HOME/fish/config.fish}.")
  (environment-variables
   (alist '())
   "Association list of environment variables to set in Fish."
   serialize-fish-env-vars)
  (aliases
   (alist '())
   "Association list of aliases for Fish, both the key and the value
should be a string.  An alias is just a simple function that wraps a
command, If you want something more akin to @dfn{aliases} in POSIX
shells, see the @code{abbreviations} field."
   serialize-fish-aliases)
  (abbreviations
   (alist '())
   "Association list of abbreviations for Fish.  These are words that,
when typed in the shell, will automatically expand to the full text."
   serialize-fish-abbreviations))

(define (fish-files-service config)
  `(("fish/config.fish"
     ,(mixed-text-file
       "fish-config.fish"
       #~(string-append "\
# if we haven't sourced the login config, do it
status --is-login; and not set -q __fish_login_config_sourced
and begin
  set --prepend fish_function_path "
                        #$fish-foreign-env
                        "/share/fish/functions
  fenv source $HOME/.profile
  set -e fish_function_path[1]
  set -g __fish_login_config_sourced 1
end\n\n")
       (serialize-configuration
        config
        home-fish-configuration-fields)))))

(define (fish-profile-service config)
  (list (home-fish-configuration-package config)))

(define-configuration/no-serialization home-fish-extension
  (config
   (text-config '())
   "List of file-like objects for extending the Fish initialization file.")
  (environment-variables
   (alist '())
   "Association list of environment variables to set.")
  (aliases
   (alist '())
   "Association list of Fish aliases.")
  (abbreviations
   (alist '())
   "Association list of Fish abbreviations."))

(define (home-fish-extensions original-config extension-configs)
  (home-fish-configuration
   (inherit original-config)
   (config
    (append (home-fish-configuration-config original-config)
            (append-map
             home-fish-extension-config extension-configs)))
   (environment-variables
    (append (home-fish-configuration-environment-variables original-config)
            (append-map
             home-fish-extension-environment-variables extension-configs)))
   (aliases
    (append (home-fish-configuration-aliases original-config)
            (append-map
             home-fish-extension-aliases extension-configs)))
   (abbreviations
    (append (home-fish-configuration-abbreviations original-config)
            (append-map
             home-fish-extension-abbreviations extension-configs)))))

;; TODO: Support for generating completion files
;; TODO: Support for installing plugins
(define home-fish-service-type
  (service-type (name 'home-fish)
                (extensions
                 (list (service-extension
                        home-xdg-configuration-files-service-type
                        fish-files-service)
                       (service-extension
                        home-profile-service-type
                        fish-profile-service)))
                (compose identity)
                (extend home-fish-extensions)
                (default-value (home-fish-configuration))
                (description "\
Install and configure Fish, the friendly interactive shell.")))
