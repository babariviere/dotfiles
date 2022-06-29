(define-module (baba home geras)
  #:use-module (baba)
  #:use-module (baba home services elixir)
  #:use-module (baba home services fonts)
  #:use-module (baba home services browsers)
  #:use-module (baba home services emacs)
  #:use-module (baba home services wm)
  #:use-module (baba packages dotnet)
  #:use-module (baba packages fonts)
  #:use-module (baba packages pingu)
  #:use-module (baba packages security)
  #:use-module (brycus package)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages node)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (nongnu packages mozilla))

;; TODO: extend service to source home-manager variables.
(home-environment
 (packages
  (list keychain
        pavucontrol

        firefox
        unzip

        dotnet-lts
        omnisharp
        node-lts

        brycus

        pingu))
 (services
  (append emacs-service
          nyxt-service
          stumpwm-service
          home-elixir-service
          home-picom-service
          (home-xsession-service stumpwm "stumpwm")
          (list
           (simple-service 'setup-nix
                           home-shell-profile-service-type
                           (list (plain-file "setup-nix"
                                             ". $HOME/.profile-nix")))
           (simple-service 'setup-dotnet
                           home-environment-variables-service-type
                           `(("PATH" . "$HOME/.dotnet/tools:$PATH")))
           (simple-service 'direnv-config
                           home-xdg-configuration-files-service-type
                           `(("direnv/lib/use_asdf.sh" ,(local-file (string-append %channel-root "/etc/direnv/lib/use_asdf.sh")))))
           (service home-font-service-type
                    (home-font-configuration
                     (sans-serif (make-font-spec font-abattis-cantarell "Cantarell"))
                     (serif (make-font-spec font-liberation "Times New Roman"))
                     (monospace (make-font-spec font-biosevka "Biosevka"))))))))
