(define-module (baba home services shells)
  #:use-module (baba)
  #:use-module (baba packages shellutils)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (home-zsh-profile))

(define home-zsh-profile
  (list
   (service home-zsh-service-type
            (home-zsh-configuration
             (zshrc
              (append
               (list
                (local-file (etc-file "/zsh/init.zsh"))
                (local-file (etc-file "/zsh/completion.zsh"))
                (local-file (etc-file "/zsh/prompt.zsh"))
                (local-file (etc-file "/zsh/config.zsh")))
               (map (lambda (p)
                      (let ((x (package-name p)))
                        (mixed-text-file (format #f "zsh-load-~a" x)
                                         "source " p "/share/zsh/plugins/" x "/" x ".zsh")))
                    (list zsh-autosuggestions zsh-completions zsh-syntax-highlighting zsh-z))
               (list
                (local-file (etc-file "/zsh/fini.zsh")))))))))
