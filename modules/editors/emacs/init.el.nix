{ config, lib, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.editors.emacs;

  filterList = list: builtins.filter (el: el != "") list;

  concatAttrsToString = sep: mapper: m:
    lib.concatStringsSep sep (filterList (lib.mapAttrsToList mapper m));

  generateFeatureFlags = prefix: feature:
    (if feature ? flags then
      prefix + (lib.concatStringsSep " " (map (s: "+" + s) feature.flags))
    else
      "");

  generateFeature = name: feature:
    let
      _hasFlags = feature ? flags;
      hasFlags = if _hasFlags then builtins.length feature.flags > 0 else false;
      surround = cond: s: if cond then "(" + s + ")" else s;
    in if feature.enable then
      "  ${surround hasFlags (name + (generateFeatureFlags " " feature))}"
    else
      "";

  generateSection = name: section:
    "  :${name}\n" + (concatAttrsToString "\n" generateFeature section);

  # Generate sections from map.
  #
  # Example:
  # {
  #   input = {
  #     chinese.enable = true;
  #     company = { enable = true; flags = [ "childframe" ]; };
  #   };
  # }
  generateSections = sections:
    concatAttrsToString "\n\n" generateSection sections;
in ''
  ;;; init.el -*- lexical-binding: t; -*-
  (doom!
'' + generateSections {
  completion = {
    "${cfg.completion}" = {
      enable = true;
      flags = if cfg.completion == "ivy" then [ "icons" ] else [ ];
    };
    company = {
      enable = true;
      flags = [ "childframe" ];
    };
  };

  ui = {
    doom.enable = true;
    doom-dashboard.enable = true;
    doom-quit.enable = true;
    hl-todo.enable = true; # highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
    modeline.enable = true;
    nav-flash.enable = true;
    ophints.enable = true;
    popup = {
      enable = true;
      flags = [ "all" "defaults" ];
    };
    pretty-code = {
      enable = true;
      flags = [ "iosevka" ];
    };
    unicode.enable = true;
    vc-gutter.enable = true;
    vi-tilde-fringe.enable = true;
    window-select.enable = true;
    workspaces.enable = true;
  };

  editor = {
    evil = {
      enable = true;
      flags = [ "everywhere" ];
    };
    file-templates.enable = true;
    fold.enable = true;
    format = {
      enable = true;
      flags = [ "onsave" ];
    };
    multiple-cursors.enable = true;
    rotate-text.enable = true;
    snippets.enable = true;
  };

  emacs = {
    dired = {
      enable = true;
      flags = [ "icons" ];
    };
    electric.enable = true;
    vc.enable = true;
  };

  term."${cfg.terminal}".enable = true;

  tools = {
    ansible.enable = dotfiles.tools.devops.enable;
    debugger.enable = true;
    direnv.enable = dotfiles.shell.direnv.enable;
    docker.enable = dotfiles.tools.docker.enable;
    editorconfig.enable = true;
    eval.enable = true;
    flycheck.enable = true;
    flyspell.enable = true;
    gist.enable = true;
    lookup = {
      enable = true;
      flags = [ "docsets" ];
    };
    lsp.enable = true;
    magit.enable = true;
    make.enable = true;
    pdf.enable = true;
    rgb.enable = true;
    terraform.enable = dotfiles.tools.devops.enable;
  };

  lang = {
    cc.enable = true;
    data.enable = true;
    emacs-lisp.enable = true;
    go = {
      enable = dotfiles.dev.go.enable;
      flags = [ "lsp" ];
    };
    haskell = {
      enable = dotfiles.dev.haskell.enable;
      flags = [ "lsp" ];
    };
    # TODO: latex
    markdown.enable = true;
    nix.enable = true;
    org = {
      enable = true;
      flags = [ "ipython" "pandoc" "present" ];
    };
    rust = {
      enable = dotfiles.dev.rust.enable;
      flags = [ "lsp" ];
    };
    sh.enable = true;
  };

  # email = { };

  # app = { };
} + "\n\n" + generateSection "config" {
  default = {
    enable = true;
    flags = [ "bindings" "smartparens" ];
  };
} + ")"
