{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.ruby;
in {
  options.dotfiles.dev.ruby.enable = lib.mkEnableOption "ruby";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs.unstable; [
      ruby
      solargraph
      rubocop
    ];
  };
}
