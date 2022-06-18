(define-module (baba home geras)
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
  #:use-module (gnu packages node)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (nongnu packages mozilla))

;; TODO: extend service to source home-manager variables.
(home-environment
 (packages
  (list keychain
        pavucontrol
        flatpak
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
          (list
           (simple-service 'setup-nix
                           home-shell-profile-service-type
                           (list (plain-file "setup-nix"
                                             ". $HOME/.profile-nix")))
           (simple-service 'setup-dotnet
                           home-environment-variables-service-type
                           `(("PATH" . "$HOME/.dotnet/tools:$PATH")))
           (service home-font-service-type
                    (home-font-configuration
                     (sans-serif (make-font-spec font-inter "Inter"))
                     (serif (make-font-spec font-liberation "Times New Roman"))
                     (monospace (make-font-spec font-biosevka "Biosevka"))))
           (simple-service 'setup-flatpak
                           home-environment-variables-service-type
                           `(("XDG_DATA_DIRS" . "$HOME/.local/share/flatpak/exports/share:$XDG_DATA_DIRS")
                             ("PATH" . "$HOME/.local/share/flatpak/exports/bin:$PATH")))))))
