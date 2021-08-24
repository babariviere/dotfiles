{ emacs, emacsPackagesFor }:

let emacsWithPackages = (emacsPackagesFor emacs).emacsWithPackages;
in emacsWithPackages (epkgs:
  with epkgs; [
    doom-modeline
    helpful
    use-package
    which-key

    # Completion
    vertico
    marginalia
    orderless
    consult
    embark
    embark-consult

    projectile

    evil
    evil-collection
    general

    eldoc

    magit
    magit-todos
    forge

    diredfl
    all-the-icons-dired
    diff-hl

    lispy
    lispyville

    # Company
    company
    company-box

    # Lsp
    lsp-mode
    lsp-ui
    consult-lsp

    # Nix
    nix-mode
    nix-update

    # Direnv
    envrc

    # Elisp
    eros
    highlight-quoted
    elisp-demos
    macrostep

    # Elixir
    elixir-mode
    alchemist

    # Smartparens
    smartparens

    # Snippets
    yasnippet
    yasnippet-snippets

    # Themes
    doom-themes
    kaolin-themes
  ])
