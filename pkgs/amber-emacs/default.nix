{ emacs, emacsPackagesFor, lib, makeWrapper,
  # external dependencies
  direnv,
  nixfmt,
  ripgrep }:

let
  emacs' = emacs.overrideAttrs (old: {
    buildInputs = old.buildInputs or [] ++ [ makeWrapper ];
    postInstall = old.postInstall or "" + ''
    wrapProgram $out/bin/emacs \
      --prefix PATH : ${lib.makeBinPath [ nixfmt ripgrep ]}
  '';
  });
  emacsWithPackages = (emacsPackagesFor emacs').emacsWithPackages;
in
emacsWithPackages (epkgs:
  with epkgs; [
    doom-modeline
    helpful
    use-package
    which-key
    gcmh

    # Completion
    vertico
    marginalia
    orderless
    consult
    embark
    embark-consult

    # Project
    projectile

    # Keys
    evil
    evil-collection
    general

    eldoc

    # Git
    magit
    magit-todos
    forge

    # Dired
    diredfl
    all-the-icons-dired
    diff-hl

    # Lisp
    lispy
    lispyville

    # Company
    company
    company-box

    # Data
    yaml-mode

    # Direnv
    direnv
    envrc

    # Elisp
    eros
    highlight-quoted
    elisp-demos
    macrostep

    # Elixir
    elixir-mode
    alchemist

    # Format
    format-all

    # Lsp
    lsp-mode
    lsp-ui
    consult-lsp

    # Org
    org
    org-appear
    org-contrib
    org-superstar
    evil-org

    # Nix
    nix-mode
    nix-update

    # Smartparens
    smartparens

    # Snippets
    yasnippet
    yasnippet-snippets

    # Themes
    doom-themes
    kaolin-themes

    # vterm
    vterm
  ])
