{ emacs, emacsPackagesFor, lib, makeWrapper, fetchFromGitHub, writeText
, runCommand }:

let
  overrides = final: prev: {
    doom-snippets = final.melpaBuild {
      pname = "doom-snippets";
      ename = "doom-snippets";
      recipe = writeText "recipe" ''
        (doom-snippets :fetcher github
                       :repo "hlissner/doom-snippets"
                       :files ("*.el" "*-mode" ".nosearch"))
      '';
      version = "20210730.0";
      commit = "772e0fcaeee9b17ab3dfe10feccfa1b5199c4f60";
      src = let
        src = fetchFromGitHub {
          owner = "hlissner";
          repo = "doom-snippets";
          rev = "772e0fcaeee9b17ab3dfe10feccfa1b5199c4f60";
          sha256 = "1g99z0ws0jswh892f4cf62rkjl4jinjg63sl59m0z7f3n5hi9dc1";
        };
      in runCommand "doom-snippets-src" { } ''
        cp -R ${src} $out
        chmod +w $out/*
        for d in $out/*/; do
            touch "$d/.nosearch"
        done
      '';

    };
    yaml = prev.yaml.overrideAttrs (old: {
      # Disable native comp as it keeps getting stuck
      postInstall = "true";
    });
  };
  emacsWithPackages = ((emacsPackagesFor emacs).overrideScope'
    overrides).emacs.pkgs.emacsWithPackages;
in emacsWithPackages (epkgs:
  with epkgs; [
    doom-modeline
    helpful
    use-package
    which-key
    gcmh
    hl-todo
    git-gutter
    git-gutter-fringe
    exec-path-from-shell

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
    evil-surround
    evil-commentary
    general
    undo-fu
    undo-fu-session

    eldoc

    # Git
    magit
    magit-todos
    forge

    # Dired
    diredfl
    all-the-icons-dired
    diff-hl

    # Common Lisp
    sly
    sly-macrostep
    sly-repl-ansi-color

    # Lisp
    lispy
    lispyville

    # Company
    company
    company-box

    # Csharp
    csharp-mode

    # Data
    yaml-mode
    gitlab-ci-mode

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
    exunit

    # Fish
    fish-mode

    # Flycheck
    flycheck
    flycheck-inline

    # Format
    format-all

    # Go
    go-mode
    go-gen-test
    go-guru

    # Lsp
    lsp-mode
    lsp-ui
    consult-lsp

    # Org
    org
    org-appear
    org-contrib
    org-edna
    org-superstar
    evil-org
    org-roam

    # Nix
    nix-mode
    nix-update

    # Smartparens
    smartparens

    # Snippets
    yasnippet
    doom-snippets

    # Themes
    doom-themes
    kaolin-themes

    # vterm
    vterm

    # Misc
    git-auto-commit-mode
  ])
