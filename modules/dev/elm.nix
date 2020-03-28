{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.elm;
in {
  options.dotfiles.dev.elm.enable = lib.mkEnableOption "elm";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs.unstable.elmPackages; [
      elm
      elm-coverage
      elm-doc-preview
      elm-format
      elm-language-server
      elm-live
      elm-test
      elm-upgrade
      elm-verify-examples
    ];
  };
}
