{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.tools.aws;
in {
  options.dotfiles.tools.aws.enable = lib.mkEnableOption "aws";

  # TODO: manage aws/config aws/credentials ?
  config =
    lib.mkIf cfg.enable { environment.systemPackages = with pkgs; [ awscli ]; };
}
