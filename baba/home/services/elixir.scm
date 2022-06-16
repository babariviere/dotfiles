(define-module (baba home services elixir)
  #:use-module (gnu home services)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages elixir)
  #:use-module (gnu services)
  #:export (home-elixir-service))

(define home-elixir-service
  (list (simple-service 'elixir-packages
                        home-profile-service-type
                        (list erlang elixir))
        (simple-service 'elixir-setup
                        home-environment-variables-service-type
                        `(("PATH" . "$HOME/.mix/escripts:$PATH")))))
