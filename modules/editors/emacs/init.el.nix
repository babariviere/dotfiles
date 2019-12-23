{ config, lib }:

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
    in if name != "name" && feature.enable then
      "  ${surround hasFlags (name + (generateFeatureFlags " " feature))}"
    else
      "";

  generateSection = section:
    "  :${section.name}\n" + (concatAttrsToString "\n" generateFeature section);

  # Generate sections from map.
  #
  # Example:
  # [
  #   ({
  #     name = "input";
  #     chinese.enable = true;
  #     company = { enable = true; flags = [ "childframe" ]; };
  #   })
  # }
  generateSections = sections:
    lib.concatStringsSep "\n\n" (filterList (map generateSection sections));

in let
  header = ''
    ;;; init.el -*- lexical-binding: t; -*-
    (doom!
  '';

in header + (generateSections [
  {
    name = "completion";
    "${cfg.completion}" = {
      enable = true;
      flags = if cfg.completion == "ivy" then [ "icons" ] else [ ];
    };
    company = {
      enable = true;
      flags = [ "childframe" ];
    };
  }

  {
    name = "ui";
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
  }

  {
    name = "editor";
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
  }

  {
    name = "emacs";
    dired = {
      enable = true;
      flags = [ "icons" ];
    };
    electric.enable = true;
    vc.enable = true;
  }

  {
    name = "term";
    "${cfg.terminal}".enable = true;
  }

  {
    name = "tools";
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
  }

  {
    name = "lang";
    cc = {
      enable = true;
      flags = [ "lsp" ];
    };
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
      flags = [
        "brain"
        "gnuplot"
        "hugo"
        "journal"
        "jupyter"
        "pandoc"
        "pomodoro"
        "present"
      ];
    };
    rust = {
      enable = dotfiles.dev.rust.enable;
      flags = [ "lsp" ];
    };
    sh.enable = true;
  }

  {
    name = "email";
    notmuch.enable = dotfiles.services.mail.enable;
  }

  {
    name = "config";
    default = {
      enable = true;
      flags = [ "bindings" "smartparens" ];
    };
  }
]) + ")"
