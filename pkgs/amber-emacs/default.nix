{ emacs, emacsPackagesFor, lib, makeWrapper, fetchFromGitHub, writeText
, runCommand }:

let
  emacsWithPackages = (emacsPackagesFor emacs).emacs.pkgs.emacsWithPackages;
in emacsWithPackages (epkgs:
  with epkgs; [
	  helpful
	  use-package
	  which-key
	  gcmh
	  hl-todo
	  git-gutter
	  git-gutter-fringe
	  exec-path-from-shell

    # completion
	  vertico
	  marginalia
	  orderless
	  consult
	  embark
	  embark-consult

    general
	  undo-fu
	  undo-fu-session
    vundo
	  eldoc

    # magit
	  magit
    magit-section
	  forge

    # dired
	  diredfl
	  all-the-icons-dired
	  diff-hl

    # common lisp
	  sly
    sly-asdf
	  sly-macrostep
	  sly-repl-ansi-color

    # lisp
	  lispy

    # c/c++
	  ccls
	  cmake-mode
	  demangle-mode
	  disaster
	  modern-cpp-font-lock

    # check
    flymake-diagnostic-at-point

    # corfu
    corfu
    cape
    kind-icon
    corfu-doc
    pcmpl-args

    # snippets
    tempel
    yasnippet

    # csharp
	  csharp-mode
    csproj-mode
    sharper

    # data
    json-mode
    yaml-mode
	  gitlab-ci-mode

    # direnv
	  envrc

    # docker
    docker
    dockerfile-mode

    # emacs lisp
	  eros
	  highlight-quoted
	  elisp-demos
	  macrostep
	  macrostep-geiser

    # erlang
    erlang

    # elixir
    elixir-mode
    alchemist
    exunit

    # fish
	  fish-mode

    # go
	  go-mode
	  go-guru

    # haskell
    haskell-mode
    hindent

    # lsp
	  eglot

    # elfeed
    elfeed
    elfeed-org

    # org-mode
	  org
	  org-appear
	  org-contrib
	  org-edna
	  org-superstar
	  org-roam
    org-roam-ui

    # nix
	  nix-mode
	  nix-update

    # python
    python-mode
	  poetry
    blacken

    # parens
	  smartparens

    # themes
	  modus-themes

    # shell
	  vterm

    # scheme
	  geiser
	  guix
	  geiser-guile

    # misc
	  emacsql-sqlite3
	  editorconfig
	  git-auto-commit-mode
	  zoom
	  crux
	  htmlize
    diminish

    # navigation
	  avy
	  ace-window

    # http
    verb

    # services
    slack
    notmuch
  ])
