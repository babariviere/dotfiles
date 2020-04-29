{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.web;
in {
  options.dotfiles.dev.web.enable = lib.mkEnableOption "web";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs.unstable.nodePackages;
      [ vscode-css-languageserver-bin vscode-html-languageserver-bin ]
      ++ (lib.optional dotfiles.dev.ocaml.enable pkgs.bs-platform);
  };
}
